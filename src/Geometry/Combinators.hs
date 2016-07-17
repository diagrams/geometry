{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Combinators
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Higher-level tools for combining diagrams.
--
-----------------------------------------------------------------------------

module Geometry.Combinators
  (
    -- * Binary operations
    beside
  , atDirection

    -- * n-ary operations
  , appends
  , position, atPoints
  , cat , cat'
  , CatOpts(_catMethod, _sep), catMethod, sep
  , CatMethod(..)
  -- , composeAligned

  ) where

import           Control.Lens              hiding (beside, ( # ))
import           Data.Default.Class
import           Data.Monoid.WithSemigroup
import           Data.Proxy
import           Data.Semigroup

import           Geometry.Direction
import           Geometry.Juxtapose
import           Geometry.Space
import           Geometry.Transform

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

------------------------------------------------------------
-- Combining two objects
------------------------------------------------------------

-- | @beneath@ is just a convenient synonym for @'flip' 'atop'@; that is,
--   @d1 \`beneath\` d2@ is the diagram with @d2@ superimposed on top of
--   @d1@.
-- beneath :: (Metric v, HasBasis v, Foldable v, OrderedField n, Monoid' m)
--      => QDiagram b v n m -> QDiagram b v n m -> QDiagram b v n m
-- beneath = flip atop

-- infixl 6 `beneath`

-- | Place two monoidal objects (/i.e./ diagrams, paths,
--   animations...) next to each other along the given vector.  In
--   particular, place the second object so that the vector points
--   from the local origin of the first object to the local origin of
--   the second object, at a distance so that their envelopes are just
--   tangent.  The local origin of the new, combined object is the
--   local origin of the first object (unless the first object is the
--   identity element, in which case the second object is returned
--   unchanged).
--
--   <<diagrams/src_Diagrams_Combinators_besideEx.svg#diagram=besideEx&height=200>>
--
--   > besideEx = beside (r2 (20,30))
--   >                   (circle 1 # fc orange)
--   >                   (circle 1.5 # fc purple)
--   >            # showOrigin
--   >            # centerXY # pad 1.1
--
--   Note that @beside v@ is associative, so objects under @beside v@
--   form a semigroup for any given vector @v@.  In fact, they also
--   form a monoid: 'mempty' is clearly a right identity (@beside v d1
--   mempty === d1@), and there should also be a special case to make
--   it a left identity, as described above.
--
--   In older versions of diagrams, @beside@ put the local origin of
--   the result at the point of tangency between the two inputs.  That
--   semantics can easily be recovered by performing an alignment on
--   the first input before combining.  That is, if @beside'@ denotes
--   the old semantics,
--
--   > beside' v x1 x2 = beside v (x1 # align v) x2
--
--   To get something like @beside v x1 x2@ whose local origin is
--   identified with that of @x2@ instead of @x1@, use @beside
--   (negateV v) x2 x1@.
beside :: (Juxtaposable a, Semigroup a) => Vn a -> a -> a -> a
beside v d1 d2 = d1 <> juxtapose v d1 d2

-- | Place two diagrams (or other juxtaposable objects) adjacent to
--   one another, with the second diagram placed in the direction 'd'
--   from the first.  The local origin of the resulting combined
--   diagram is the same as the local origin of the first.  See the
--   documentation of 'beside' for more information.
atDirection :: (InSpace v n a, Metric v, Floating n, Juxtaposable a, Semigroup a)
            => Direction v n -> a -> a -> a
atDirection = beside . fromDirection

------------------------------------------------------------
-- Combining multiple objects
------------------------------------------------------------

-- | @appends x ys@ appends each of the objects in @ys@ to the object
--   @x@ in the corresponding direction.  Note that each object in
--   @ys@ is positioned beside @x@ /without/ reference to the other
--   objects in @ys@, so this is not the same as iterating 'beside'.
--
--   <<diagrams/src_Diagrams_Combinators_appendsEx.svg#diagram=appendsEx&width=200>>
--
--   > appendsEx = appends c (zip (iterateN 6 (rotateBy (1/6)) unitX) (repeat c))
--   >             # centerXY # pad 1.1
--   >   where c = circle 1
appends :: (Juxtaposable a, Monoid' a) => a -> [(Vn a,a)] -> a
appends d1 apps = d1 <> mconcat (map (\(v,d) -> juxtapose v d1 d) apps)

-- | Position things absolutely: combine a list of objects
--   (e.g. diagrams or paths) by assigning them absolute positions in
--   the vector space of the combined object.
--
--   <<diagrams/src_Diagrams_Combinators_positionEx.svg#diagram=positionEx&height=300>>
--
--   > positionEx = position (zip (map mkPoint [-3, -2.8 .. 3]) (repeat spot))
--   >   where spot      = circle 0.2 # fc black
--   >         mkPoint :: Double -> P2 Double
--   >         mkPoint x = p2 (x,x*x)
position :: (InSpace v n a, HasOrigin a, Monoid' a) => [(Point v n, a)] -> a
position = mconcat . map (uncurry moveTo)

-- | Curried version of @position@, takes a list of points and a list of
--   objects.
atPoints :: (InSpace v n a, HasOrigin a, Monoid' a) => [Point v n] -> [a] -> a
atPoints ps as = position $ zip ps as

-- | Methods for concatenating diagrams.
data CatMethod
  = Cat
  -- ^ Normal catenation: simply put diagrams next to one another
  -- (possibly with a certain distance in between each). The distance
  -- between successive diagram /envelopes/ will be consistent; the
  -- distance between /origins/ may vary if the diagrams are of
  -- different sizes.
  | Distrib
  -- ^ Distribution: place the local origins of diagrams at regular
  -- intervals.  With this method, the distance between successive
  -- /origins/ will be consistent but the distance between envelopes may
  -- not be.  Indeed, depending on the amount of separation, diagrams
  -- may overlap.

-- | Options for 'cat''.
data CatOpts n = CatOpts
  { _catMethod    :: CatMethod
  , _sep          :: n
  , catOptsvProxy :: Proxy n
  }

-- The reason the proxy field is necessary is that without it,
-- altering the sep field could theoretically change the type of a
-- CatOpts record.  This causes problems when using record update, as
-- in @with { _sep = 10 }@, because knowing the type of the whole
-- expression does not tell us anything about the type of @with@, and
-- therefore the @Num (Scalar v)@ constraint cannot be satisfied.
-- Adding the Proxy field constrains the type of @with@ in @with {_sep
-- = 10}@ to be the same as the type of the whole expression.  Note
-- this is not a problem when using the 'sep' lens, as its type is
-- more restricted.

makeLensesWith (lensRules & generateSignatures .~ False) ''CatOpts

-- | Which 'CatMethod' should be used:
--   normal catenation (default), or distribution?
catMethod :: Lens' (CatOpts n) CatMethod

-- | How much separation should be used between successive diagrams
--   (default: 0)?  When @catMethod = Cat@, this is the distance between
--   /envelopes/; when @catMethod = Distrib@, this is the distance
--   between /origins/.
sep :: Lens' (CatOpts n) n

instance Num n => Default (CatOpts n) where
  def = CatOpts
    { _catMethod    = Cat
    , _sep          = 0
    , catOptsvProxy = Proxy
    }

-- | @cat v@ positions a list of objects so that their local origins
--   lie along a line in the direction of @v@.  Successive objects
--   will have their envelopes just touching.  The local origin
--   of the result will be the same as the local origin of the first
--   object.
--
--   See also 'cat'', which takes an extra options record allowing
--   certain aspects of the operation to be tweaked.
cat :: (InSpace v n a, Metric v, Floating n, Juxtaposable a, Monoid' a, HasOrigin a)
       => v n -> [a] -> a
cat v = cat' v def

-- | Like 'cat', but taking an extra 'CatOpts' arguments allowing the
--   user to specify
--
--   * The spacing method: catenation (uniform spacing between
--     envelopes) or distribution (uniform spacing between local
--     origins).  The default is catenation.
--
--   * The amount of separation between successive diagram
--     envelopes/origins (depending on the spacing method).  The
--     default is 0.
--
--   'CatOpts' is an instance of 'Default', so 'with' may be used for
--   the second argument, as in @cat' (1,2) (with & sep .~ 2)@.
--
--   Note that @cat' v (with & catMethod .~ Distrib) === mconcat@
--   (distributing with a separation of 0 is the same as
--   superimposing).
-- cat' :: (InSpace v n a, Metric v, Floating n, Juxtaposable a, Monoid' a, HasOrigin a)
--      => v n -> CatOpts n -> [a] -> a
-- cat' :: (InSpace v n t, Juxtaposable t, HasOrigin t, Monoid' t, Metric v, Floating n)
--      => Direction v n -> CatOpts n -> [t] -> t
-- cat' _ _ []                          = []
-- cat' _ _ [t]                         = [t]
-- cat' v (CatOpts method s) tss@(_:ts) =
--   case method of
--     Cat     ->
--       let sv      = -s ^* signorm v
--           f d1 d2 = d1 <> (juxtapose v d1 d2 & moveOriginBy sv)
--       in  zipWith f tss ts
--     Distrib -> distribV (s *^ getDir d)

cat' v (CatOpts { _catMethod = Cat, _sep = s }) = foldB comb mempty
  where comb d1 d2 = d1 <> (juxtapose v d1 d2 & moveOriginBy vs)
        vs = s *^ signorm (negated v)
cat' v (CatOpts { _catMethod = Distrib, _sep = s }) =
  position . zip (iterate (.+^ (s *^ signorm v)) origin)

-- | Given an associative binary operation and a default value to use
--   in the case of an empty list, perform a /balanced/ fold over a
--   list.  For example,
--
--   @
--   foldB (+) z [a,b,c,d,e,f] == ((a+b) + (c+d)) + (e+f)
--   @
--
foldB :: (a -> a -> a) -> a -> [a] -> a
foldB _ z [] = z
foldB f _ as = foldB' as
  where foldB' [x] = x
        foldB' xs  = foldB' (go xs)
        go []         = []
        go [x]        = [x]
        go (x1:x2:xs) = f x1 x2 : go xs

-- | Compose a list of diagrams using the given composition function,
--   first aligning them all according to the given alignment, *but*
--   retain the local origin of the first diagram, as it would be if
--   the composition function were applied directly.  That is,
--   @composeAligned algn comp@ is equivalent to @translate v . comp
--   . map algn@ for some appropriate translation vector @v@.
--
--   Unfortunately, this only works for diagrams (and not, say, paths)
--   because there is no most general type for alignment functions,
--   and no generic way to find out what an alignment function does to
--   the origin of things.  (However, it should be possible to make a
--   version of this function that works /specifically/ on paths, if
--   such a thing were deemed useful.)
--
--   <<#diagram=alignedEx1&width=400>>
--
--   > alignedEx1 = (hsep 2 # composeAligned alignT) (map circle [1,3,5,2])
--   >            # showOrigin
--
--   <<#diagram=alignedEx2&width=400>>
--
--   > alignedEx2 = (mconcat # composeAligned alignTL) [circle 1, square 1, triangle 1, pentagon 1]
--   >            # showOrigin
-- composeAligned
--   :: (Monoid' m, HasBasis v, Foldable v, Floating n, Ord n, Metric v)
--   => (QDiagram b v n m -> QDiagram b v n m)    -- ^ Alignment function
--   -> ([QDiagram b v n m] -> QDiagram b v n m)  -- ^ Composition function
--   -> ([QDiagram b v n m] -> QDiagram b v n m)
-- composeAligned _ combine [] = combine []
-- composeAligned algn comb (d:ds) = (comb $ map algn (d:ds)) # moveOriginTo l
--   where
--     mss = ( (() .>> d)   -- qualify first to avoid stomping on an existing () name
--           # named ()     -- Mark the origin
--           # algn         -- Apply the alignment function
--           )
--           -- then find out what happened to the origin
--         ^. subMap . _Wrapped . Control.Lens.at (toName ())
--     l   = location . head . fromJust $ mss
--           -- the fromJust is Justified since we put the () name in
