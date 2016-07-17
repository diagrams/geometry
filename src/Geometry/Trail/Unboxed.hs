{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Trail
-- Copyright   :  (c) 2013-2016 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module defines /trails/, translationally invariant paths
-- through space.  Trails form a central part of the diagrams-lib API,
-- so the documentation for this module merits careful study.
--
-- Related modules include:
--
-- * The 'TrailLike' class ("Diagrams.TrailLike") exposes a generic
--   API for building a wide range of things out of trails.
--
-- * 'Path's ("Diagrams.Path") are collections of 'Located'
--   ("Diagrams.Located") trails.
--
-- * Trails are composed of 'Segment's (see "Diagrams.Segment"),
--   though most users should not need to work with segments directly.
--
-----------------------------------------------------------------------------

module Geometry.Trail.Unboxed
  (
    -- * Trails
    UTrail (..)
  , UTrail' (..)
  , Line, Loop
  , Unboxable
    -- * ClosedSegent
  , ClosedSegment (..)
  , getEnv

    -- * Vector internals
    -- ** Segment vectors
  , SegVector(..)

    -- ** Folds
  , foldSegments
  , foldSegmentsT
  , unboxEnvelope
  , U.Vector(V_CSeg)
  , U.MVector(MV_CSeg)
  ) where

-- import           Control.Arrow            ((***))
import           Control.Lens             hiding (at, transform, (<|), (|>))
-- import           Data.Sequence            as S (Seq, (<|), (|>))
-- import qualified Data.Sequence            as S
-- import           Data.Fixed
import qualified Data.Foldable            as F
-- import           Data.Monoid.MList
import           Data.Semigroup
-- import qualified Numeric.Interval.Kaucher as I
import qualified Data.Vector.Unboxed as U

import Geometry.TwoD.Transform
import           Geometry.Space
import           Geometry.Located
import           Geometry.Angle
import           Geometry.Trace
import           Geometry.Transform
import Geometry.Query
import           Geometry.Envelope
import           Geometry.Parametric
import           Geometry.Segment
-- import           Geometry.Tangent
import           Geometry.Trail (Line, Loop, Trail' (..), withTrail, SegTree (..))
import           Geometry.TwoD.Path (Crossings (..))

import Data.Coerce

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector
import           Linear.V2
import           Linear.V3

-- import           Data.Serialize            (Serialize)
-- import qualified Data.Serialize            as Serialize

-- import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U
import Data.Word
import Control.Monad (liftM, liftM2)
import Control.Applicative (liftA2)

import Geometry.TrailLike
-- import Geometry.Located
import Geometry.TwoD.Vector

import Data.List (maximumBy)
import Data.Ord (comparing)

import Prelude hiding ((^))
import qualified Prelude

(^) :: Num a => a -> Int -> a
(^) = (Prelude.^)

-- -- Diagrams instances --------------------------------------------------

-- type instance V (SegVector v n) = v
-- type instance N (SegVector v n) = n

-- instance (HasLinearMap v, Num n) => Transformable (SegVector v n) where
--   transform t (SegVector u) = SegVector (U.map (transform t) u)

-- Special closed segment ----------------------------------------------

data ClosedSegment v n
  = LSeg !(v n)
  | CSeg !(v n) !(v n) !(v n)

type instance V (ClosedSegment v n) = v
type instance N (ClosedSegment v n) = n
type instance Codomain (ClosedSegment v n) = v

instance (HasLinearMap v, Num n) => Transformable (ClosedSegment v n) where
  transform t (LSeg v) = LSeg (apply t v)
  transform t (CSeg v1 v2 v3) = CSeg (apply t v1) (apply t v2) (apply t v3)
  {-# INLINE transform #-}

data instance U.Vector (ClosedSegment v n) =
  V_CSeg !(U.Vector Word8) !(U.Vector (v n))

data instance U.MVector s (ClosedSegment v n) =
  MV_CSeg !(U.MVector s Word8) !(U.MVector s (v n))

instance U.Unbox (v n) => U.Unbox (ClosedSegment v n)

instance U.Unbox (v n) => M.MVector U.MVector (ClosedSegment v n) where
  basicLength (MV_CSeg w _) =
    M.basicLength w
  {-# INLINE basicLength #-}
  basicUnsafeSlice m n (MV_CSeg w v) =
    MV_CSeg (M.basicUnsafeSlice m n w) (M.basicUnsafeSlice (3*m) (3*n) v)
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_CSeg _ v1) (MV_CSeg _ v2) =
    M.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n =
    liftM2 MV_CSeg (M.basicUnsafeNew n) (M.basicUnsafeNew (3*n))
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeRead (MV_CSeg w v) i = do
    word <- M.basicUnsafeRead w i
    let !o = 3*i
    case word of
      -- Linear
      0 -> liftM LSeg (M.basicUnsafeRead v o)
      -- Cubic
      _ -> do
        v1 <- M.basicUnsafeRead v o
        v2 <- M.basicUnsafeRead v (o+1)
        v3 <- M.basicUnsafeRead v (o+2)
        return $ CSeg v1 v2 v3
      -- _ -> error "Malformed unboxed segment"
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_CSeg w v) i a = do
    let !o = 3*i
    case a of
      LSeg v1 -> do
        M.basicUnsafeWrite w i 0
        M.basicUnsafeWrite v o v1
      CSeg v1 v2 v3 -> do
        M.basicUnsafeWrite w i 1
        M.basicUnsafeWrite v o v1
        M.basicUnsafeWrite v (o+1) v2
        M.basicUnsafeWrite v (o+2) v3
  {-# INLINE basicUnsafeWrite #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_CSeg w v) = M.basicInitialize w >> M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif

instance U.Unbox (v n) => G.Vector U.Vector (ClosedSegment v n) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (MV_CSeg w v) =
    liftM2  V_CSeg (G.basicUnsafeFreeze w) (G.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_CSeg w v) =
    liftM2 MV_CSeg (G.basicUnsafeThaw w)   (G.basicUnsafeThaw   v)
  basicLength       ( V_CSeg w _) = G.basicLength w
  basicUnsafeSlice m n (V_CSeg w v) =
    V_CSeg (G.basicUnsafeSlice m n w) (G.basicUnsafeSlice (3*m) (3*n) v)
  basicUnsafeIndexM (V_CSeg w v) i = do
    word <- G.basicUnsafeIndexM w i
    let !o = 3*i
    case word of
      0 -> liftM LSeg (G.basicUnsafeIndexM v o)
      1 -> do
        v1 <- G.basicUnsafeIndexM v o
        v2 <- G.basicUnsafeIndexM v (o+1)
        v3 <- G.basicUnsafeIndexM v (o+2)
        return $ CSeg v1 v2 v3
      _ -> error "Malformed unboxed segment"

instance (Additive v, Num n) => Parametric (ClosedSegment v n) where
  atParam (LSeg x) t       = t *^ x
  atParam (CSeg c1 c2 x2) t = (3 * t'*t'*t ) *^ c1
                           ^+^ (3 * t'*t *t ) *^ c2
                           ^+^ (    t *t *t ) *^ x2
    where t' = 1-t
  {-# INLINE atParam #-}

getEnv :: (Metric v, OrderedField n) => ClosedSegment v n -> v n -> n -- Envelope v n
getEnv s = \v -> -- mkEnvelope $ \v ->
  let n = case s of
        LSeg l -> l `dot` v
        CSeg c1 c2 c3 ->
          let quadSol =
                filterNode (liftA2 (&&) (>0) (<1)) $
                quadForm' (3 * ((3 *^ c1 ^-^ 3 *^ c2 ^+^ c3) `dot` v))
                          (6 * (((-2) *^ c1 ^+^ c2) `dot` v))
                          ((3 *^ c1) `dot` v)
              f t = (s `atParam` t) `dot` v
          in maxNode (c3 `dot` v) (mapNode f quadSol)
  in  if n > 0 then n / quadrance v else 0
{-# INLINE getEnv #-}
{-# SPECIALISE INLINE getEnv :: ClosedSegment V2 Double -> V2 Double -> Double #-}
{-# SPECIALISE INLINE getEnv :: ClosedSegment V3 Double -> V3 Double -> Double #-}

data Node a
  = None
  | One !a
  | Two !a !a

mapNode :: (a -> b) -> Node a -> Node b
mapNode _ None      = None
mapNode f (One a)   = One (f a)
mapNode f (Two a b) = Two (f a) (f b)
{-# INLINE mapNode #-}

maxNode :: Ord a => a -> Node a -> a
maxNode a = \case
  None    -> a
  One b   -> max a b
  Two b c -> max (max a b) c
{-# INLINE maxNode #-}

filterNode :: (a -> Bool) -> Node a -> Node a
filterNode f = \case
  -- None    -> None
  -- One a   -> if f a then One a else None
  None                -> None
  One a   | f a       -> One a
          | otherwise -> None
  Two a b | f a       -> if f b then Two a b else One a
          | f b       -> One b
          | otherwise -> None
{-# INLINE filterNode #-}

nodeList :: Node a -> [a]
nodeList = \case
  None    -> []
  One a   -> [a]
  Two a b -> [a,b]

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

aboutZero' :: (Ord n, Num n) => n -> n -> Bool
aboutZero' tol n = abs n < tol

-- | Solve the cubic equation ax^3 + bx^2 + cx + d = 0, returning a
--   list of all real roots. First argument is tolerance.
cubForm'' :: OrderedField a => a -> a -> a -> a -> a -> [a]
cubForm'' toler a b c d
  | aboutZero' toler a      = nodeList $ quadForm' b c d

    -- three real roots, use trig method to avoid complex numbers
  | delta >  0              = map trig [0,1,2]

    -- one real root of multiplicity 3
  | delta == 0 && disc == 0 = [ -b/(3*a) ]

    -- two real roots, one of multiplicity 2
  | delta == 0 && disc /= 0 = [ (b*c - 9*a*d)/(2*disc)
                              , (9*a^2*d - 4*a*b*c + b^3)/(a * disc)
                              ]

    -- one real root (and two complex)
  | otherwise               = [-b/(3*a) - cc/(3*a) + disc/(3*a*cc)]

  where
    delta  = 18*a*b*c*d - 4*b*b*b*d + b^2*c*c - 4*a*c*c*c - 27*a*a*d*d
    disc   = 3*a*c - b*b
    qq     = sqrt(-27*(a*a)*delta)
    qq'    | aboutZero' toler disc = maximumBy (comparing (abs . (+xx))) [qq, -qq]
           | otherwise             = qq
    cc     = cubert (1/2*(qq' + xx))
    xx     = 2*b*b*b - 9*a*b*c + 27*a^2*d
    p      = disc/(3*a*a)
    q      = xx/(27*a*a*a)
    phi = 1/3*acos(3*q/(2*p)*sqrt(-3/p))
    trig k = 2 * sqrt(-p/3) * cos(phi - 2*k*pi/3) - b/(3*a)
    cubert x | x < 0     = -((-x)**(1/3))
             | otherwise = x**(1/3)


------------------------------------------------------------------------
-- Folds overs segments
------------------------------------------------------------------------

-- | Fold over the segments in a path.
foldSegments :: (Additive v, Num n, Semigroup m, U.Unbox (v n))
  => (Point v n -> v n -> m) -- ^ line to
  -> (Point v n -> v n -> v n -> v n -> m) -- ^ curve to
  -> Point v n -- start
  -> U.Vector (ClosedSegment v n) -- trail
  -> m -- ^ start
  -> (Point v n, m) -- ^ result
foldSegments sf cf p0 (V_CSeg ws vs) = go 0 p0 where
  go i !p m | i == n = (p,m)
  go i !p m = case U.unsafeIndex ws i of
    0 -> let !v  = U.unsafeIndex vs o
             !p' = p .+^ v
         in  go (i+1) p' (m <> sf p v)
    _ -> let !v1 = U.unsafeIndex vs o
             !v2 = U.unsafeIndex vs (o+1)
             !v3 = U.unsafeIndex vs (o+2)
             !p' = p .+^ v3
         in  go (i+1) p' (m <> cf p v1 v2 v3)
    where o = 3*i
  !n = U.length ws
{-# INLINE foldSegments #-}
-- This style of function is used because it has a much better chance of
-- being unboxed. Using a predefined fold from vector over the Closed
-- Segment doesn't seem to unbox, it just packs and unpacks it, costing
-- a considerable performing wise.

-- | Fold over the segments in a path.
foldSegmentsT :: (HasLinearMap v, Num n, Semigroup m, U.Unbox (v n))
  => (Point v n -> m) -- ^ line to
  -> (Point v n -> Point v n -> Point v n -> m) -- ^ curve to
  -> Transformation v n -- ^ transformation to apply to vector
  -> Point v n -- ^ starting point (already transformed)
  -> U.Vector (ClosedSegment v n) -- trail
  -> m -- ^ start
  -> (Point v n, m) -- ^ (end point, result)
foldSegmentsT sf cf t p0 (V_CSeg ws vs) = go 0 p0 where
  go i !p m | i == n = (p,m)
  go i !p m = case U.unsafeIndex ws i of
    0 -> let !v  = apply t (U.unsafeIndex vs o)
             !p' = p .+^ v
         in  go (i+1) p' (m <> sf p')
    _ -> let !v1 = apply t (U.unsafeIndex vs o)
             !v2 = apply t (U.unsafeIndex vs (o+1))
             !v3 = apply t (U.unsafeIndex vs (o+2))
             !p' = p .+^ v3
         in  go (i+1) p' (m <> cf (p .+^ v1) (p .+^ v2) p')
    where o = 3*i
  !n = U.length ws
{-# INLINE foldSegmentsT #-}

-- segments :: (HasLinearMap v, Num n, Semigroup m, U.Unbox (v n))
--          => IndexedFold (Point v n) U.Vector (ClosedSegment v n) (ClosedSegment v n)
-- segments = ifoldring XXX

-- segmentsT :: (HasLinearMap v, Num n, Semigroup m, U.Unbox (v n))
--          => Transformation v n -> Point v n -> IndexedFold (Point v n) (ClosedSegment v n)
-- segmentsT = ifoldring XXX

-- | Fold over the segments in a path.
-- foldSegmentsT :: (HasLinearMap v, Num n, Semigroup m, U.Unbox (v n))
--   => (Point v n -> v n -> m) -- ^ line to
--   -> (Point v n -> v n -> v n -> v n -> m) -- ^ curve to
--   -> Transformation v n
--   -> Point v n -- ^ starting point (already transformed)
--   -> U.Vector (ClosedSegment v n) -- trail
--   -> m -- ^ start
--   -> (Point v n, m) -- ^ (end point, result)
-- foldSegmentsT sf cf t p0 (V_CSeg ws vs) = go 0 p0 where
--   go i !p m | i == n = (p,m)
--   go i !p m = case U.unsafeIndex ws i of
--     0 -> let !v  = apply t (U.unsafeIndex vs o)
--              !p' = p .+^ v
--          in  go (i+1) p' (m <> sf p v)
--     _ -> let !v1 = apply t (U.unsafeIndex vs o)
--              !v2 = apply t (U.unsafeIndex vs (o+1))
--              !v3 = apply t (U.unsafeIndex vs (o+2))
--              !p' = p .+^ v3
--          in  go (i+1) p' (m <> cf p v1 v2 v3)
--     where o = 3*i
--   !n = U.length ws
-- {-# INLINE foldSegmentsT #-}

-- This style of function is used because it has a much better chance of
-- being unboxed. Using a predefined fold from vector over the Closed
-- Segment doesn't seem to unbox, it just packs and unpacks it, costing
-- a considerable ammount performing wise.

unboxEnvelope' :: (Metric v, OrderedField n, U.Unbox (v n))
  => (U.Vector (ClosedSegment v n)) -> v n -> (Point v n, n)
unboxEnvelope' uv w = coerce $ foldSegments str cuv zero uv 0
  where
  str (P p) v        = Max $ getEnv (LSeg v) w + (p `dot` w)
  cuv (P p) v1 v2 v3 = Max $ getEnv (CSeg v1 v2 v3) w + (p `dot` w)
  {-# INLINE str #-}
  {-# INLINE cuv #-}
{-# INLINEABLE [0] unboxEnvelope' #-}
{-# SPECIALISE unboxEnvelope' ::
      U.Vector (ClosedSegment V2 Double) -> V2 Double -> (Point V2 Double, Double) #-}
{-# SPECIALISE unboxEnvelope' ::
      U.Vector (ClosedSegment V3 Double) -> V3 Double -> (Point V3 Double, Double) #-}

-- | An envelope function for a trail.
unboxEnvelope :: (Metric v, OrderedField n, U.Unbox (v n))
  => (U.Vector (ClosedSegment v n)) -> v n -> n
unboxEnvelope uv w = getMax . snd $ foldSegments str cuv zero uv 0
  where
  str (P p) v        = Max $ getEnv (LSeg v) w + (p `dot` w)
  cuv (P p) v1 v2 v3 = Max $ getEnv (CSeg v1 v2 v3) w + (p `dot` w)
  {-# INLINE str #-}
  {-# INLINE cuv #-}
{-# INLINEABLE [0] unboxEnvelope #-}
{-# SPECIALISE unboxEnvelope :: U.Vector (ClosedSegment V2 Double) -> V2 Double -> Double #-}
{-# SPECIALISE unboxEnvelope :: U.Vector (ClosedSegment V3 Double) -> V3 Double -> Double #-}
-- This is the point where we stop inlining directly. The core of the
-- specialised verions of this function is quite big and there
-- wouldn't really be any benifit inlining it. The INLINEABLE pragma is
-- so this function can be specialsed for other types outside this
-- module.

-- | A sequence of closed segments stored in a vector.
newtype SegVector v n = SegVector (U.Vector (ClosedSegment v n))

type instance V (SegVector v n) = v
type instance N (SegVector v n) = n

instance (Metric v, OrderedField n, U.Unbox (v n)) => Enveloped (SegVector v n) where
  getEnvelope (SegVector s) = Envelope $ unboxEnvelope s
  {-# INLINE getEnvelope #-}

instance Unboxable v n => Transformable (SegVector v n) where
  transform t (SegVector s) = SegVector (U.map (transform t) s)
  {-# INLINE transform #-}

type instance Codomain (SegVector v n) = v

data UTrail' c v n where
  ULine :: SegVector v n -> UTrail' Line v n
  ULoop :: SegVector v n -> Segment Open v n -> UTrail' Line v n

type Unboxable v n = (Metric v, HasLinearMap v, OrderedField n, U.Unbox (v n))

type instance V (UTrail' c v n) = v
type instance N (UTrail' c v n) = n

instance Unboxable v n => Transformable (UTrail' c v n) where
  transform t (ULine s)   = ULine (transform t s)
  transform t (ULoop s o) = ULoop (transform t s) (transform t o)
  {-# INLINE transform #-}

instance Unboxable v n => Enveloped (UTrail' c v n) where
  getEnvelope (ULine s)               = getEnvelope s
  getEnvelope (ULoop (SegVector s) o) = Envelope $ \v ->
    let (!p, !n) = unboxEnvelope' s v
    in  max n 0 -- XXX
  {-# INLINE getEnvelope #-}

data UTrail v n where
  UTrail :: UTrail' c v n -> UTrail v n

type instance V (UTrail v n) = v
type instance N (UTrail v n) = n

instance Unboxable v n => Transformable (UTrail v n) where
  transform t (UTrail s) = UTrail (transform t s)
  {-# INLINE transform #-}

instance Unboxable v n => Enveloped (UTrail v n) where
  getEnvelope (UTrail s) = getEnvelope s
  {-# INLINE getEnvelope #-}

instance Unboxable v n => TrailLike (UTrail v n) where
  trailLike (Loc _ t) = UTrail (fromT t)
    where
      fromT = withTrail (\(Line st)   -> ULine (fromSeg st))
                        (\(Loop st o) -> ULoop (fromSeg st) o)
      fromSeg = SegVector . U.fromList . map t2t . F.toList . op SegTree

      t2t :: Segment Closed v n -> ClosedSegment v n
      t2t (Linear (OffsetClosed v))       = LSeg v
      t2t (Cubic c1 c2 (OffsetClosed c3)) = CSeg c1 c2 c3

------------------------------------------------------------------------
-- 2D
------------------------------------------------------------------------
-- foldSegments :: (Additive v, Num n, Semigroup m, U.Unbox (v n))
--   => (Point v n -> v n -> m) -- ^ line to
--   -> (Point v n -> v n -> v n -> v n -> m) -- ^ curve to
--   -> Point v n -- start
--   -> U.Vector (ClosedSegment v n) -- trail
--   -> m -- ^ start
--   -> (Point v n, m) -- ^ result

foldTrail
  :: (Additive v, Num n, Semigroup m, U.Unbox (v n))
  => (Point v n -> v n -> m) -- ^ line to
  -> (Point v n -> v n -> v n -> v n -> m) -- ^ curve to
  -> Located (UTrail v n)
  -> m -- ^ start
  -> (Point v n, m) -- ^ result
foldTrail sf cf (Loc p0 t) m = case t of
  UTrail (ULine (SegVector s))   -> foldSegments sf cf p0 s m
  UTrail (ULoop (SegVector s) o) ->
    let (p, m') = foldSegments sf cf p0 s m
    in  case o of
          Linear OffsetOpen      -> (p0, m' <> sf p (p .-. p0))
          Cubic v1 v2 OffsetOpen -> (p0, m' <> cf p v1 v2 (p .-. p0))
{-# INLINE foldTrail #-}

-- | Compute the sum of signed crossings of a trail starting from the
--   given point in the positive x direction.
trailCrossings
  :: (U.Unbox n, OrderedField n)
  => Point V2 n -> Located (UTrail V2 n) -> Crossings

-- non-loop trails have no inside or outside, so don't contribute crossings
trailCrossings _ (Loc _ (UTrail (ULine _))) = 0
trailCrossings p @ (P (V2 x y)) tr
  = snd $ foldTrail testLinear testCubic tr 0
  where

    testLinear a @(P(V2 _ ay)) v@(V2 _ vy)
      | ay <= y && vy > 0 && isLeft     = 1
      | vy <= 0 && ay > y && not isLeft = -1
      | otherwise                       = 0
      where isLeft = cross2 v (p .-. a) > 0

    testCubic a@(P(V2 _ ay)) c1@(V2 _ c1y) c2@(V2 _ c2y) c3@(V2 _ c3y)
      -- = sum . fmap tTest . filterNode (\t -> t >= 0 && t <= 1) $ ts
      = sum . fmap tTest . filter (\t -> t >= 0 && t <= 1) $ ts
      where
        ts = cubForm'' 1e-8 ( 3*c1y - 3*c2y + c3y)
                     (-6*c1y + 3*c2y)
                     ( 3*c1y)
                     (ay - y)
        tTest t = let (V2 vx _) = CSeg c1 c2 c3 `atParam` t
                  in  if vx > 0 then signFromDerivAt t else 0
        signFromDerivAt t =
          let V2 tx ty =  (3*t*t) *^ (3*^c1 ^-^ 3*^c2 ^+^ c3)
                      ^+^ (2*t)   *^ (-6*^c1 ^+^ 3*^c2)
                      ^+^            (3*^c1)
              ang = atan2A' ty tx ^. rad
          in  if | 0   < ang && ang < pi && t < 1 ->  1
                 | -pi < ang && ang < 0  && t > 0 -> -1
                 | otherwise                      ->  0
{-# SPECIALISE trailCrossings :: Point V2 Double -> Located (UTrail V2 Double) -> Crossings #-}

instance (U.Unbox n, OrderedField n)
  => HasQuery (Located (UTrail V2 n)) Crossings where
  getQuery t = Query $ \p -> trailCrossings p t
  {-# INLINE getQuery #-}

type P2 = Point V2

trailTrace
  :: (U.Unbox n, OrderedField n)
  => Located (UTrail V2 n) -> P2 n -> V2 n -> SortedList n
trailTrace t p v@(V2 vx vy) = snd $ foldTrail traceLinear traceCubic t mempty
  where
    traceLinear q w
      | x1 == 0 && x2 /= 0 = mempty  -- parallel
      | otherwise          = unsafeMkSortedList [t] -- intersecting or collinear
      where
        t  = x3 / x1
        x1 =  v `cross2` w
        x2 = pq `cross2` v
        x3 = pq `cross2` w
        pq  = q .-. p

    theta = atan2A' vy vx
    rot   = rotation theta
    t2    = rotation theta <> translation (p^._Point) <> scaling (1/norm v)

    traceCubic q c1 c2 c3 = mkSortedList ts -- completely wrong
      where
        qy = papply t2 q ^. _y
        y1 = apply t2 c1 ^. _y
        y2 = apply t2 c2 ^. _y
        y3 = apply t2 c3 ^. _y
        --
        a  =  3*y1 - 3*y2 + y3
        b  = -6*y1 + 3*y2
        c  =  3*y1
        d  =  qy
        ts = filter (liftA2 (&&) (>= 0) (<= 1)) (cubForm'' 1e-8 a b c d)
{-# SPECIALISE trailTrace :: Located (UTrail V2 Double) -> P2 Double -> V2 Double -> SortedList Double #-}

instance (U.Unbox n, OrderedField n) => Traced (UTrail V2 n) where
  getTrace t = mkTrace $ trailTrace (t `at` origin)

-- solveCubic :: n -> n -> n -> n -> Node n
-- solveCubic y c1 c2 c3
--   where
--     a  =    3*c1 - 3*c2 + c3
--     b  =  - 6*c1 + 3*c2
--     c  =    3*c1
--     d  = c0
--     ts = filter (liftA2 (&&) (>= 0) (<= 1)) (cubForm a b c d)

