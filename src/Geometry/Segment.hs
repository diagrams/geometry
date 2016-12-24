{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE UndecidableInstances       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Segment
-- Copyright   :  (c) 2011-2016 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A /segment/ is a translation-invariant, atomic path.  Currently,
-- there are two types: linear (/i.e./ just a straight line to the
-- endpoint) and cubic Bézier curves (/i.e./ a curve to an endpoint
-- with two control points).  This module contains tools for creating
-- and manipulating segments, as well as a definition of segments with
-- a fixed location (useful for backend implementors).
--
-- Generally speaking, casual users of diagrams should not need this
-- module; the higher-level functionality provided by
-- "Diagrams.Trail", "Diagrams.TrailLike", and "Diagrams.Path" should
-- usually suffice.  However, directly manipulating segments can
-- occasionally be useful.
--
-----------------------------------------------------------------------------

module Geometry.Segment
  (
    -- * Segment offsets
    -- ** Open/closed tags
    Open, Closed
  , Offset(..)
  , HasOffset (..)

    -- * Constructing and modifying segments

  , Segment(..), straight, bezier3, bézier3
  , openLinear, openCubic

    -- * Fixed (absolutely located) segments
  , FixedSegment(..)
  , fixed

    -- * Internals

  ) where

import           Control.Lens hiding (at)
-- import Data.Primitive.MutVar
-- import Data.Primitive
-- import Control.Monad.Primitive
-- import           Data.Monoid.MList
import           Control.Monad
-- import           Data.Semigroup
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
-- import qualified Data.Vector.Unboxed.Base as U
import           Data.Word
import           Numeric.Interval.Kaucher (Interval (..))
import qualified Numeric.Interval.Kaucher as I

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector
import Linear (V2, V3) -- specialise instances only

-- import           Control.Applicative
import Data.Functor.Classes

-- import           Diagrams.Solve
import           Geometry.Located
import           Geometry.Transform
import           Geometry.Envelope
-- import           Geometry.Trace
-- import           Geometry.HasOrigin
import           Geometry.Parametric
import           Geometry.Space

------------------------------------------------------------------------
-- Open/closed type tags
------------------------------------------------------------------------

-- Eventually we should use DataKinds for this, but not until we drop
-- support for GHC 7.4.

-- | Type tag for open segments.
data Open
-- | Type tag for closed segments.
data Closed

------------------------------------------------------------------------
-- Segment offsets
------------------------------------------------------------------------

-- | The /offset/ of a segment is the vector from its starting point
--   to its end.  The offset for an /open/ segment is determined by
--   the context, /i.e./ its endpoint is not fixed.  The offset for a
--   /closed/ segment is stored explicitly, /i.e./ its endpoint is at
--   a fixed offset from its start.
-- data Offset c v n where
-- data Offset :: OffsetType -> (* -> *) -> * -> * where
data Offset c v n where
  OffsetOpen   :: Offset Open v n
  OffsetClosed :: !(v n) -> Offset Closed v n

-- deriving instance Show1 v => Show1 (Offset c v)
-- deriving instance Eq1 v => Eq1 (Offset c v)
-- deriving instance Ord1 v => Ord1 (Offset c v)

-- deriving instance (Show1 v, Show n) => Show (Offset c v n)
instance (Eq1 v, Eq n) => Eq (Offset c v n) where
  OffsetOpen      == OffsetOpen      = True
  OffsetClosed v1 == OffsetClosed v2 = eq1 v1 v2

instance Functor v => Functor (Offset c v) where
  fmap _ OffsetOpen       = OffsetOpen
  fmap f (OffsetClosed v) = OffsetClosed (fmap f v)
  {-# INLINE fmap #-}

instance Each (Offset c v n) (Offset c v' n') (v n) (v' n') where
  each f (OffsetClosed v) = OffsetClosed <$> f v
  each _ OffsetOpen       = pure OffsetOpen
  {-# INLINE each #-}

-- | Reverses the direction of closed offsets.
instance (Additive v, Num n) => Reversing (Offset c v n) where
  reversing (OffsetClosed off) = OffsetClosed $ negated off
  reversing a@OffsetOpen       = a
  {-# INLINE reversing #-}

type instance V (Offset c v n) = v
type instance N (Offset c v n) = n

instance (HasLinearMap v, Num n) => Transformable (Offset c v n) where
  transform _ OffsetOpen       = OffsetOpen
  transform t (OffsetClosed v) = OffsetClosed (apply t v)
  {-# INLINE transform #-}

------------------------------------------------------------
--  Constructing segments  ---------------------------------
------------------------------------------------------------

-- | The atomic constituents of the concrete representation currently
--   used for trails are /segments/, currently limited to
--   single straight lines or cubic Bézier curves. Segments are
--   /translationally invariant/, that is, they have no particular
--   \"location\" and are unaffected by translations.  They are,
--   however, affected by other transformations such as rotations and
--   scales.
data Segment c v n
  = Linear !(Offset c v n)
    -- ^ A linear segment with given offset.

  | Cubic !(v n) !(v n) !(Offset c v n)
    -- ^ A cubic Bézier segment specified by three offsets from the
    --   starting point to the first control point, second control
    --   point, and ending point, respectively.
  deriving (Functor, Eq)
  -- deriving (Functor, Eq, Ord)

type instance V (Segment c v n) = v
type instance N (Segment c v n) = n

instance Show1 v => Show1 (Segment c v) where
  liftShowsPrec sp sl d seg = case seg of
    Linear (OffsetClosed v)       -> showsUnaryWith (liftShowsPrec sp sl) "straight" d v
    Cubic v1 v2 (OffsetClosed v3) -> showParen (d > 10) $
      showString "bézier3  " . liftShowsPrec sp sl 11 v1 . showChar ' '
                             . liftShowsPrec sp sl 11 v2 . showChar ' '
                             . liftShowsPrec sp sl 11 v3
    Linear OffsetOpen             -> showString "openLinear"
    Cubic v1 v2 OffsetOpen        -> showParen (d > 10) $
      showString "openCubic " . liftShowsPrec sp sl 11 v1 . showChar ' '
                              . liftShowsPrec sp sl 11 v2

instance Each (Segment c v n) (Segment c v' n') (v n) (v' n') where
  each f (Linear o)      = Linear <$> each f o
  each f (Cubic v1 v2 o) = Cubic  <$> f v1 <*> f v2 <*> each f o
  {-# INLINE each #-}

instance (HasLinearMap v, Num n) => Transformable (Segment c v n) where
  transform = over each . apply
  {-# INLINE transform #-}

instance (Additive v, Num n) => Reversing (Segment Closed v n) where
  reversing = \case
    Linear (OffsetClosed v)       -> straight (negated v)
    Cubic c1 c2 (OffsetClosed x2) -> bezier3 (c2 ^-^ x2) (c1 ^-^ x2) (negated x2)
  {-# INLINE reversing #-}

instance (Show1 v, Show n) => Show (Segment c v n) where
  showsPrec = showsPrec1

data instance U.Vector (Segment Closed v n) = V_Seg !(U.Vector Word8)  !(U.Vector (v n))
data instance U.MVector s (Segment Closed v n) = MV_Seg !(U.MVector s Word8) !(U.MVector s (v n))
instance U.Unbox (v n) => U.Unbox (Segment Closed v n)

instance U.Unbox (v n) => M.MVector U.MVector (Segment Closed v n) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  basicLength (MV_Seg w _) =
    M.basicLength w
  basicUnsafeSlice m n (MV_Seg w v) =
    MV_Seg (M.basicUnsafeSlice m n w) (M.basicUnsafeSlice (3*m) (3*n) v)
  basicOverlaps (MV_Seg _ v1) (MV_Seg _ v2) =
    M.basicOverlaps v1 v2
  basicUnsafeNew n =
    liftM2 MV_Seg (M.basicUnsafeNew n) (M.basicUnsafeNew (3*n))
  basicUnsafeRead (MV_Seg w v) i = do
    word <- M.basicUnsafeRead w i
    let !o = 3*i
    case word of
      -- Linear
      0 -> liftM straight (M.basicUnsafeRead v o)
      -- Cubic
      1 -> do
        v1 <- M.basicUnsafeRead v o
        v2 <- M.basicUnsafeRead v (o+1)
        v3 <- M.basicUnsafeRead v (o+2)
        return $ bezier3 v1 v2 v3
      _ -> error "Malformed unboxed segment"
  basicUnsafeWrite (MV_Seg w v) i a = do
    let !o = 3*i
    case a of
      Linear (OffsetClosed v1) -> do
        M.basicUnsafeWrite w i 0
        M.basicUnsafeWrite v o v1
      Cubic v1 v2 (OffsetClosed v3) -> do
        M.basicUnsafeWrite w i 1
        M.basicUnsafeWrite v o v1
        M.basicUnsafeWrite v (o+1) v2
        M.basicUnsafeWrite v (o+2) v3
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Seg w v) = M.basicInitialize w >> M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif

instance U.Unbox (v n) => G.Vector U.Vector (Segment Closed v n) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (MV_Seg w v) =
    liftM2  V_Seg (G.basicUnsafeFreeze w) (G.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_Seg w v) =
    liftM2 MV_Seg (G.basicUnsafeThaw w)   (G.basicUnsafeThaw   v)
  basicLength       ( V_Seg w _) = G.basicLength w
  basicUnsafeSlice m n (V_Seg w v) =
    V_Seg (G.basicUnsafeSlice m n w) (G.basicUnsafeSlice (3*m) (3*n) v)
  basicUnsafeIndexM (V_Seg w v) i = do
    word <- G.basicUnsafeIndexM w i
    let !o = 3*i
    case word of
      0 -> liftM (Linear . OffsetClosed) (G.basicUnsafeIndexM v o)
      1 -> do
        v1 <- G.basicUnsafeIndexM v o
        v2 <- G.basicUnsafeIndexM v (o+1)
        v3 <- G.basicUnsafeIndexM v (o+2)
        return $ Cubic v1 v2 (OffsetClosed v3)
      _ -> error "Malformed unboxed segment"

-- | @'straight' v@ constructs a translationally invariant linear
--   segment with direction and length given by the vector @v@.
straight :: v n -> Segment Closed v n
straight = Linear . OffsetClosed
{-# INLINE straight #-}

-- Note, if we didn't have a Linear constructor we could also create
-- linear segments with @Cubic (v ^/ 3) (2 *^ (v ^/ 3)) v@.  Those
-- would not be precisely the same, however, since we can actually
-- observe how segments are parametrized.

-- | @bezier3 c1 c2 x@ constructs a translationally invariant cubic
--   Bézier curve where the offsets from the first endpoint to the
--   first and second control point and endpoint are respectively
--   given by @c1@, @c2@, and @x@.
bezier3 :: v n -> v n -> v n -> Segment Closed v n
bezier3 c1 c2 x = Cubic c1 c2 (OffsetClosed x)
{-# INLINE bezier3 #-}

-- | @bézier3@ is the same as @bezier3@, but with more snobbery.
bézier3 :: v n -> v n -> v n -> Segment Closed v n
bézier3 = bezier3
{-# INLINE bézier3 #-}

type instance Codomain (Segment Closed v n) = v

-- | 'atParam' yields a parametrized view of segments as continuous
--   functions @[0,1] -> v@, which give the offset from the start of
--   the segment for each value of the parameter between @0@ and @1@.
--   It is designed to be used infix, like @seg ``atParam`` 0.5@.
instance (Additive v, Num n) => Parametric (Segment Closed v n) where
  atParam (Linear (OffsetClosed x)) t       = t *^ x
  atParam (Cubic c1 c2 (OffsetClosed x2)) t =     (3 * t'*t'*t ) *^ c1
                                              ^+^ (3 * t'*t *t ) *^ c2
                                              ^+^ (    t *t *t ) *^ x2
    where t' = 1-t
  {-# INLINE atParam #-}

instance (Additive v, Num n) => Parametric (Tangent (Segment Closed v n)) where
  Tangent (Linear (OffsetClosed v)) `atParam` _ = v
  Tangent (Cubic c1 c2 (OffsetClosed x2)) `atParam` p
    = (3*(3*p*p-4*p+1))*^c1 ^+^ (3*(2-3*p)*p)*^c2 ^+^ (3*p*p)*^x2

instance (Additive v, Num n) => EndValues (Tangent (Segment Closed v n)) where
  atStart (Tangent (Linear (OffsetClosed v)))      = v
  atStart (Tangent (Cubic c1 _ _))                 = c1
  atEnd   (Tangent (Linear (OffsetClosed v)))      = v
  atEnd   (Tangent (Cubic _ c2 (OffsetClosed x2))) = x2 ^-^ c2


instance Num n => DomainBounds (Segment Closed v n)

instance (Additive v, Num n) => EndValues (Segment Closed v n) where
  atStart                            = const zero
  {-# INLINE atStart #-}
  atEnd (Linear (OffsetClosed v))    = v
  atEnd (Cubic _ _ (OffsetClosed v)) = v
  {-# INLINE atEnd #-}

class HasOffset a where
  -- | Lens onto the total offset of something. Changing the offset is
  --   acheived by uniform scaling and rotation. Only a valid lens for
  --   non-zero offsets.
  offset :: Lens' a (Vn a)

-- maybe we can only do this for V2?
instance HasOffset (Segment Closed v n) where
  offset f (Linear (OffsetClosed v))       = straight <$> f v
  offset f (Cubic c1 c2 (OffsetClosed c3)) = bezier3 c1 c2 <$> f c3
  {-# INLINE offset #-}

instance HasOffset a => HasOffset (Located a) where
  offset = located . offset
  {-# INLINE offset #-}

-- | An open linear segment. This means the trail makes a straight line
--   from the last segment the beginning to form a loop.
openLinear :: Segment Open v n
openLinear = Linear OffsetOpen

-- | An open cubic segment. This means the trail makes a cubic bézier
--   with control vectors @v1@ and @v2@ to form a loop.
openCubic :: v n -> v n -> Segment Open v n
openCubic v1 v2 = Cubic v1 v2 OffsetOpen

------------------------------------------------------------
--  Computing segment envelope  ------------------------------
------------------------------------------------------------

{- 3 (1-t)^2 t c1 + 3 (1-t) t^2 c2 + t^3 x2

   Can we compute the projection of B(t) onto a given vector v?

   u.v = |u||v| cos th

   |proj_v u| = cos th * |u|
              = (u.v/|v|)

   so B_v(t) = (B(t).v/|v|)

   Then take the derivative of this wrt. t, get a quadratic, solve.

   B_v(t) = (1/|v|) *     -- note this does not affect max/min, can solve for t first
            3 (1-t)^2 t (c1.v) + 3 (1-t) t^2 (c2.v) + t^3 (x2.v)
          = t^3 ((3c1 - 3c2 + x2).v) + t^2 ((-6c1 + 3c2).v) + t (3c1.v)

   B_v'(t) = t^2 (3(3c1 - 3c2 + x2).v) + t (6(-2c1 + c2).v) + 3c1.v

   Set equal to zero, use quadratic formula.
-}

-- | The envelope for a segment is based at the segment's start.
instance (Metric v, OrderedField n) => Enveloped (Segment Closed v n) where

--   getEnvelope @(Linear (OffsetClosed t)) = mkEnvelope $ \v ->
--     -- maximum (map (\t -> (s `atParam` t) `dot` v) [0,1]) / quadrance v
--     let n = (t `dot` v)
--     in  if n > 0 then n / quadrance v else 0

--   getEnvelope (s@(Cubic c1 c2 (OffsetClosed x2))) = mkEnvelope $ \v ->
--     maximum .
--     map (\t -> ((s `atParam` t) `dot` v) / quadrance v) $
--     [0,1] ++
--     filter (liftA2 (&&) (>0) (<1))
--       (quadForm (3 * ((3 *^ c1 ^-^ 3 *^ c2 ^+^ x2) `dot` v))
--                 (6 * (((-2) *^ c1 ^+^ c2) `dot` v))
--                 ((3 *^ c1) `dot` v))

  getEnvelope = getEnv
  -- getEnvelope s = mkEnvelope $ \v ->
  --   let n = case s of
  --         Linear (OffsetClosed l) -> l `dot` v
  --         Cubic c1 c2 (OffsetClosed c3) ->
  --           let quadSol =
  --                 quadForm' (3 * ((3 *^ c1 ^-^ 3 *^ c2 ^+^ c3) `dot` v))
  --                           (6 * (((-2) *^ c1 ^+^ c2) `dot` v))
  --                           ((3 *^ c1) `dot` v)
  --               f t = (s `atParam` t) `dot` v
  --           in max (maximum (fmap f quadSol)) (c3 `dot` v)
  --   in  if n > 0 then n / quadrance v else 0
  --   -- n = max
  {-# INLINE getEnvelope #-}


getEnv :: (Metric v, OrderedField n) => Segment Closed v n -> Envelope v n
getEnv s = mkEnvelope $ \v ->
  let n = case s of
        Linear (OffsetClosed l) -> l `dot` v
        Cubic c1 c2 (OffsetClosed c3) ->
          let quadSol = filterNode (\t -> t >= 0 && t < 1) $
                quadForm' (3 * ((3 *^ c1 ^-^ 3 *^ c2 ^+^ c3) `dot` v))
                          (6 * (((-2) *^ c1 ^+^ c2) `dot` v))
                          ((3 *^ c1) `dot` v)
              f t = (s `atParam` t) `dot` v
          in maxNode (c3 `dot` v) (mapNode f quadSol)
  in  if n > 0 then n / quadrance v else 0
{-# INLINE [0] getEnv #-}
{-# SPECIALISE INLINE getEnv :: Segment Closed V2 Double -> Envelope V2 Double #-}
{-# SPECIALISE INLINE getEnv :: Segment Closed V3 Double -> Envelope V3 Double #-}

data Node a
  = None
  | One !a
  | Two !a !a

mapNode :: (a -> b) -> Node a -> Node b
mapNode _ None = None
mapNode f (One a) = One (f a)
mapNode f (Two a b) = Two (f a) (f b)
{-# INLINE mapNode #-}

maxNode :: Ord a => a -> Node a -> a
maxNode a = \case
  None -> a
  One b -> max a b
  Two b c -> max (max a b) c
{-# INLINE maxNode #-}

filterNode :: (a -> Bool) -> Node a -> Node a
filterNode f = \case
  None    -> None
  One a   -> if f a then One a else None
  Two a b | f a       -> if f b then Two a b else One a
          | otherwise -> if f b then One a else None
{-# INLINE filterNode #-}

-- | The quadratic formula.
quadForm' :: OrderedField n => n -> n -> n -> Node n
quadForm' a b c
    -- There are infinitely many solutions in this case,
    -- so arbitrarily return 0
  | a == 0 && b == 0 && c == 0 = One 0
  | a == 0 && b == 0 = None                -- c /= 0
  | a == 0           = One (-c/b)          -- linear
  | d < 0            = None                -- no real solutions
  | b == 0           = let s = sqrt (-c/a) -- ax^2 + c = 0
                       in  Two s (-s)
  | d == 0           = One (-b/(2*a))      -- multiplicity 2 solution
    -- see http://www.mpi-hd.mpg.de/astrophysik/HEA/internal/Numerical_Recipes/f5-6.pdf
  | otherwise        = Two (q/a) (c/q)
 where
   d = b*b - 4*a*c
   q = -1/2*(b + signum b * sqrt d)
{-# INLINE quadForm' #-}

------------------------------------------------------------
--  Manipulating segments
------------------------------------------------------------

instance (Additive v, Fractional n) => Sectionable (Segment Closed v n) where
  splitAtParam (Linear (OffsetClosed x1)) t = (left, right)
    where left  = straight p
          right = straight (x1 ^-^ p)
          p = lerp t x1 zero
  splitAtParam (Cubic c1 c2 (OffsetClosed x2)) t = (left, right)
    where left  = bezier3 a b e
          right = bezier3 (c ^-^ e) (d ^-^ e) (x2 ^-^ e)
          p = lerp t c2 c1
          a = lerp t c1 zero
          b = lerp t p a
          d = lerp t x2 c2
          c = lerp t d p
          e = lerp t c b
  {-# INLINE splitAtParam #-}

  reverseDomain = reversing
  {-# INLINE reverseDomain #-}

instance (Metric v, OrderedField n)
      => HasArcLength (Segment Closed v n) where

  arcLengthBounded _ (Linear (OffsetClosed x1)) = I.singleton $ norm x1
  arcLengthBounded m s@(Cubic c1 c2 (OffsetClosed x2))
    | ub - lb < m = I lb ub
    | otherwise   = arcLengthBounded (m/2) l + arcLengthBounded (m/2) r
   where (l,r) = s `splitAtParam` 0.5
         ub    = sum (map norm [c1, c2 ^-^ c1, x2 ^-^ c2])
         lb    = norm x2

  arcLengthToParam m s _ | arcLength m s == 0 = 0.5
  arcLengthToParam m s@(Linear {}) len = len / arcLength m s
  arcLengthToParam m s@(Cubic {})  len
    | len `I.elem` I (-m/2) (m/2) = 0
    | len < 0              = - arcLengthToParam m (fst (splitAtParam s (-1))) (-len)
    | len `I.elem` slen    = 1
    | len > I.sup slen     = 2 * arcLengthToParam m (fst (splitAtParam s 2)) len
    | len < I.sup llen     = (*0.5) $ arcLengthToParam m l len
    | otherwise            = (+0.5) . (*0.5)
                           $ arcLengthToParam (9*m/10) r (len - I.midpoint llen)
    where (l,r) = s `splitAtParam` 0.5
          llen  = arcLengthBounded (m/10) l
          slen  = arcLengthBounded m s

  -- Note, the above seems to be quite slow since it duplicates a lot of
  -- work.  We could trade off some time for space by building a tree of
  -- parameter values (up to a certain depth...)

------------------------------------------------------------
--  Fixed segments
------------------------------------------------------------

-- | @FixedSegment@s are like 'Segment's except that they have
--   absolute locations.  @FixedSegment v@ is isomorphic to @Located
--   (Segment Closed v)@, as witnessed by 'mkFixedSeg' and
--   'fromFixedSeg', but @FixedSegment@ is convenient when one needs
--   the absolute locations of the vertices and control points.
data FixedSegment v n
  = FLinear !(Point v n) !(Point v n)
  | FCubic !(Point v n) !(Point v n) !(Point v n) !(Point v n)
  deriving (Show, Read, Eq)

type instance V (FixedSegment v n) = v
type instance N (FixedSegment v n) = n

instance (Additive v, Num n) => Parametric (Tangent (FixedSegment v n)) where
  atParam (Tangent fSeg) = atParam $ Tangent (view fixed fSeg)

instance (Additive v, Num n) => EndValues (Tangent (FixedSegment v n)) where
  atStart (Tangent fSeg) = atStart $ Tangent (view fixed fSeg)
  atEnd (Tangent fSeg)   = atEnd $ Tangent (view fixed fSeg)

instance Each (FixedSegment v n) (FixedSegment v' n') (Point v n) (Point v' n') where
  each f (FLinear p0 p1)      = FLinear <$> f p0 <*> f p1
  each f (FCubic p0 p1 p2 p3) = FCubic  <$> f p0 <*> f p1 <*> f p2 <*> f p3
  {-# INLINE each #-}

-- | Reverses the control points.
instance Reversing (FixedSegment v n) where
  reversing (FLinear p0 p1)      = FLinear p1 p0
  reversing (FCubic p0 p1 p2 p3) = FCubic p3 p2 p1 p0
  {-# INLINE reversing #-}

instance (Additive v, Foldable v, Num n) => Transformable (FixedSegment v n) where
  transform t = over each (papply t)
  {-# INLINE transform #-}

instance (Additive v, Num n) => HasOrigin (FixedSegment v n) where
  moveOriginTo o = over each (moveOriginTo o)

instance (Metric v, OrderedField n) => Enveloped (FixedSegment v n) where
  getEnvelope f = moveTo p (getEnvelope s)
    where (p, s) = viewLoc $ f ^. fixed
  {-# INLINE getEnvelope #-}

    -- Eventually we might decide it's cleaner/more efficient (?) to
    -- have all the computation in One FixedSegment instance of
    -- Envelope, and implement the Segment instance in terms of it,
    -- instead of the other way around

instance (Metric v, OrderedField n)
      => HasArcLength (FixedSegment v n) where
  arcLengthBounded m s = arcLengthBounded m (s ^. fixed)
  arcLengthToParam m s = arcLengthToParam m (s ^. fixed)

instance (Additive v, Num n) => HasOffset (FixedSegment v n) where
  offset = fixed . offset

-- | Fixed segments and a located segments are isomorphic.
fixed :: (Additive v, Num n) => Iso' (FixedSegment v n) (Located (Segment Closed v n))
fixed = iso fromFixedSeg mkFixedSeg where
{-# INLINE fixed #-}

-- | Make a fixed segment from a located segment.
mkFixedSeg :: (Additive v, Num n) => Located (Segment Closed v n) -> FixedSegment v n
mkFixedSeg = \case
  Loc p (Linear (OffsetClosed v))       -> FLinear p (p .+^ v)
  Loc p (Cubic c1 c2 (OffsetClosed x2)) -> FCubic  p (p .+^ c1) (p .+^ c2) (p .+^ x2)
{-# INLINE mkFixedSeg #-}

-- | Make a located segment from a fixed one.
fromFixedSeg :: (Additive v, Num n) => FixedSegment v n -> Located (Segment Closed v n)
fromFixedSeg = \case
  FLinear p1 p2      -> straight (p2 .-. p1) `at` p1
  FCubic x1 c1 c2 x2 -> bezier3 (c1 .-. x1) (c2 .-. x1) (x2 .-. x1) `at` x1
{-# INLINE fromFixedSeg #-}

type instance Codomain (FixedSegment v n) = Point v

instance (Additive v, Num n) => Parametric (FixedSegment v n) where
  atParam (FLinear p1 p2) t = lerp t p2 p1
  atParam (FCubic x1 c1 c2 x2) t = p3
    where p11 = lerp t c1 x1
          p12 = lerp t c2 c1
          p13 = lerp t x2 c2

          p21 = lerp t p12 p11
          p22 = lerp t p13 p12

          p3  = lerp t p22 p21
  {-# INLINE atParam #-}

instance Num n => DomainBounds (FixedSegment v n)

instance (Additive v, Num n) => EndValues (FixedSegment v n) where
  atStart (FLinear p0 _)     = p0
  atStart (FCubic  p0 _ _ _) = p0
  {-# INLINE atStart #-}
  atEnd   (FLinear _ p1)     = p1
  atEnd   (FCubic _ _ _ p1 ) = p1
  {-# INLINE atEnd #-}

instance (Additive v, Fractional n) => Sectionable (FixedSegment v n) where
  splitAtParam (FLinear p0 p1) t = (left, right)
    where left  = FLinear p0 p
          right = FLinear p  p1
          p = lerp t p1 p0
  splitAtParam (FCubic p0 c1 c2 p1) t = (left, right)
    where left  = FCubic p0 a b cut
          right = FCubic cut c d p1
          -- first round
          a   = lerp t c1 p0
          p   = lerp t c2 c1
          d   = lerp t p1 c2
          -- second round
          b   = lerp t p a
          c   = lerp t d p
          -- final round
          cut = lerp t c b

  reverseDomain (FLinear p0 p1) = FLinear p1 p0
  reverseDomain (FCubic p0 c1 c2 p1) = FCubic p1 c2 c1 p0


