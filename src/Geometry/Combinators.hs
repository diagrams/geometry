{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE BangPatterns          #-}
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
  , cat
  , sep
  , sepEven
  -- , composeAligned

   -- * Alignment
  , align
  , alignBy
  , alignBy'

   -- * Snugging
  , snug
  , snugBy

   -- * Centering
  , center
  , centerV
  , snugCenter
  , snugCenterV

  ) where

import           Data.Monoid.WithSemigroup
import           Data.Maybe (fromMaybe)
import           Data.Semigroup
import           Data.Foldable (foldl')

import           Geometry.Direction
import           Geometry.Juxtapose
import           Geometry.Trace
import           Geometry.Envelope
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
position :: (InSpace v n a, HasOrigin a, Monoid a) => [(Point v n, a)] -> a
position = mconcat . map (uncurry moveTo)

-- | Curried version of @position@, takes a list of points and a list of
--   objects.
atPoints :: (InSpace v n a, HasOrigin a, Monoid' a) => [Point v n] -> [a] -> a
atPoints ps as = position $ zip ps as

-- | @cat v@ positions a list of objects so that their local origins
--   lie along a line in the direction of @v@.  Successive objects
--   will have their envelopes just touching.  The local origin
--   of the result will be the same as the local origin of the first
--   object.
--
--   See also 'cat'', which takes an extra options record allowing
--   certain aspects of the operation to be tweaked.
cat
  :: (InSpace v n a, Enveloped a, Monoid a, HasOrigin a)
  => v n -> [a] -> a
cat v = sep v 0

sep
  :: (InSpace v n t, Monoid t, Enveloped t, HasOrigin t)
  => v n -> n -> [t] -> t
sep _              _ []      = mempty
sep (signorm -> v) s (t0:ts) = snd $ foldl' f (n0, t0) ts
  where
    -- If we come across an empty envelope treat it as a point on the
    -- origin (this isn't ideal but what else can we do? Maybe don't
    -- even move it at all?)
    extent' = fromMaybe (0,0) . extent v
    n0 = snd $ extent' t0
    f (!n, tAcc) t = (n + s - nMin + nMax, tAcc')
      where
        (nMin, nMax) = extent' t
        nStart = n + s - nMin
        tAcc' = tAcc `mappend` moveOriginTo (P $ negate nStart *^ v) t

-- | Evenly separate items along the vector @v@ at distance @s@,
--   starting at the 'origin'.
--
--   >>> sepEven unitX $ map regPoly [3..7]
--
sepEven
  :: (InSpace v n t, Metric v, Floating n, Monoid t, HasOrigin t)
  => v n -> n -> [t] -> t
sepEven (signorm -> v) s =
  position . zip (iterate (.+^ s *^ v) origin)

------------------------------------------------------------------------
-- Aligning
------------------------------------------------------------------------

-- | @alignBy v d a@ moves the origin of @a@ along the vector @v@. If @d
--   = 1@, the origin is moved to the edge of the boundary in the
--   direction of @v@; if @d = -1@, it moves to the edge of the boundary
--   in the direction of the negation of @v@.  Other values of @d@
--   interpolate linearly (so for example, @d = 0@ centers the origin
--   along the direction of @v@).
alignBy'
  :: (InSpace v n t, Fractional n, HasOrigin t)
  => (v n -> t -> Maybe (n, n)) -> v n -> n -> t -> t
alignBy' f v d t = fromMaybe t $ do
  (a,b) <- f v t
  Just $ moveOriginTo (P $ lerp' ((d + 1) / 2) b a *^ v) t
  where
    lerp' alpha a b = alpha * a + (1 - alpha) * b
  -- case f v of
  --   Just (a,b) -> moveOriginTo (lerp ((d + 1) / 2) a b) t
  --   Nothing    -> t

alignBy
  :: (InSpace v n t, Enveloped t, HasOrigin t)
  => v n -> n -> t -> t
alignBy = alignBy' extent

-- | @align v@ aligns an enveloped object along the edge in the
--   direction of @v@. That is, it moves the local origin in the
--   direction of @v@ until it is on the edge of the envelope. (Note
--   that if the local origin is outside the envelope to begin with, it
--   may have to move \"backwards\".)
align
  :: (InSpace v n t, Enveloped t, HasOrigin t)
  => v n -> t -> t
align v = alignBy v 1

-- | Version of @alignBy@ specialized to use @traceBoundary@
snugBy
  :: (InSpace v n t, Fractional n, Traced t, HasOrigin t)
  => v n -> n -> t -> t
snugBy = alignBy' traceBoundary

traceBoundary :: (InSpace v n t, Traced t) => v n -> t -> Maybe (n,n)
traceBoundary v a =
  case getSortedList $ appTrace (getTrace a) origin v of
    []        -> Nothing
    (nMin:ns) -> let !nMax = foldl (\_ x -> x) nMin ns
                 in  Just (nMin,nMax)

-- | Like align but uses trace.
snug :: (InSpace v n t, Fractional n, Traced t, HasOrigin t)
      => v n -> t -> t
snug v = snugBy v 1

-- | @centerV v@ centers an enveloped object along the direction of
--   @v@.
centerV
  :: (InSpace v n a, Enveloped a, HasOrigin a)
  => v n -> a -> a
centerV v = alignBy v 0

applyAll :: Foldable t => t (b -> b) -> b -> b
applyAll = foldr (.) id

-- | @center@ centers an enveloped object along all of its basis vectors.
center
  :: (InSpace v n a, Traversable v, Enveloped a, HasOrigin a)
  => a -> a
center = applyAll fs
  where
    fs = map centerV basis

-- | Like @centerV@ using trace.
snugCenterV
  :: (InSpace v n a, Fractional n, Traced a, HasOrigin a)
   => v n -> a -> a
snugCenterV v = snugBy v 0

-- | Like @center@ using trace.
snugCenter
  :: (InSpace v n a, Traversable v, Fractional n, HasOrigin a, Traced a)
  => a -> a
snugCenter = applyAll fs
  where
    fs = map snugCenterV basis


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