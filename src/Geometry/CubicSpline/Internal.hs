{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.CubicSpline.Internal
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A /cubic spline/ is a smooth, connected sequence of cubic curves
-- passing through a given sequence of points.  This module implements
-- a straightforward spline generation algorithm based on solving
-- tridiagonal systems of linear equations.
--
-----------------------------------------------------------------------------
module Geometry.CubicSpline.Internal
  (
    -- * Solving for spline coefficents
    cubicSplineLineTangents
  , cubicSplineLoopTangents
  , cubicSplineLineFromTangents
  , cubicSplineLoopFromTangents
  ) where


import           Control.Monad.ST
import           Data.Foldable               as F
import qualified Data.Sequence               as Seq
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U

import           Geometry.Segment
import           Geometry.Trail
import           Linear

cubicSplineLineFromTangents
  :: (V.Vector vec (v n), Additive v, Fractional n)
  => vec (v n) -- ^ offsets
  -> v n       -- ^ total offset
  -> vec (v n) -- ^ tangents (n+1 of them)
  -> Line v n
cubicSplineLineFromTangents vv off dv = Line (Seq.fromFunction n f) off
  where
    n   = V.length vv
    f i = bezier3 c1 c2 c3 where
      c1 = alpha ^/ 3
      c2 = (2*^alpha ^+^ beta)^/3
      c3 = v
      --
      alpha = x
      beta  = 3*^v ^-^ 2*^x ^-^ x'
      --
      x' = dv V.! (i+1)
      x  = dv V.! i
      v  = vv V.! i

{-# SPECIALISE
  cubicSplineLineFromTangents :: U.Vector (V2 Double) -> V2 Double -> U.Vector (V2 Double) -> Line V2 Double
  #-}

cubicSplineLoopFromTangents
  :: (V.Vector vec (v n), Additive v, Fractional n)
  => vec (v n) -- ^ offsets
  -> v n       -- ^ total offset
  -> vec (v n) -- ^ tangents (n+1 of them)
  -> Loop v n
cubicSplineLoopFromTangents vv off dv = Loop line close
  where
    n  = V.length vv
    line = cubicSplineLineFromTangents vv off dv
    close = cubicClosing c1 c2 where
      c1 = alpha ^/ 3
      c2 = (2*^alpha^+^ beta)^/3
      --
      alpha = x
      beta  = 3*^vn ^-^ 2*^x ^-^ x'
      --
      x' = dv V.! 0
      x  = dv V.! n
      vn = negated off

{-# SPECIALISE
  cubicSplineLoopFromTangents :: U.Vector (V2 Double) -> V2 Double -> U.Vector (V2 Double) -> Loop V2 Double
  #-}

-- | Get the tangents for the cubic spline of the input offsets,
--   including the tangent at the end of the last segment.
cubicSplineLineTangents
  :: forall vec v n. (V.Vector vec (v n), V.Vector vec n, Additive v, Fractional n)
  => vec (v n) -- ^ offsets
  -> vec (v n) -- ^ tangents (n+1 of them)
cubicSplineLineTangents vs = V.create $ do
  let n = V.length vs
  cv <- M.new n :: ST s (V.Mutable vec s n)
  dv <- M.new (n+1)

  let v0 = vs V.! 0
      c0 = 1/2
      d0 = (3/2) *^ v0

  M.write cv 0 c0
  M.write dv 0 d0

  let forward !i !vm1 !cm1 !dm1
        | i < n = do
            let !v = vs V.! i
            let !c = 1 / (4 - cm1)
            let !d = c *^ (3*^(v ^+^ vm1) ^-^ dm1)
            M.write cv i c
            M.write dv i d
            forward (i+1) v c d
        | otherwise = do
            let !d = (3*^vm1 ^-^ dm1) ^/ (2 - cm1)
            M.write dv i d
            pure d

  xn <- forward 1 v0 c0 d0

  let backward !i !x'
        | i < 0     = pure ()
        | otherwise = do
            d <- M.read dv i
            c <- M.read cv i
            let x = d ^-^ c*^x'
            M.write dv i x
            backward (i-1) x

  backward (n-1) xn
  pure dv

{-# SPECIALISE
  cubicSplineLineTangents :: U.Vector (V2 Double) -> U.Vector (V2 Double)
  #-}

------------------------------------------------------------------------
-- Loop
------------------------------------------------------------------------

-- | Compute the nessesary tangents for the input vectors and closing
--   segment for a natual cubic spline.
cubicSplineLoopTangents
  :: forall vec v n. (V.Vector vec (v n), V.Vector vec n, Additive v, Fractional n)
  => vec (v n) -- ^ offsets
  -> v n       -- ^ total offset
  -> vec (v n) -- ^ tangents (n+1 of them)
cubicSplineLoopTangents vs vn = V.create $ do
  let n = V.length vs
  cv <- M.new n :: ST s (V.Mutable vec s n)
  dv <- M.new (n+1)
  uv <- M.new (n+1) :: ST s (V.Mutable vec s n)

  let v0 = vs V.! 0
      c0 = 1/3
      d0 = v0 ^+^ vn
      u0 = 1/3

  M.write cv 0 c0
  M.write dv 0 d0
  M.write uv 0 u0

  let forward i !vm1 !cm1 !dm1 !um1
        | i < n = do
            let !v = vs V.! i
                !c = 1 / (4 - cm1)
                !d = c *^ (3*^(v ^+^ vm1) ^-^ dm1)
                !u = c * (negate um1)
            M.write cv i c
            M.write dv i d
            M.write uv i u
            forward (i+1) v c d u
        | otherwise = do
            let c = 1 / (3 - cm1)
                u = c * (1 - um1)
                d = c *^ (3*^(vn ^+^ vm1) ^-^ dm1)
            M.write dv i d
            M.write uv i u

  forward 1 v0 c0 d0 u0

  let backward i !_ !_ | i < 0 = pure ()
      backward i x' w' = do
        c <- M.read cv i
        d <- M.read dv i
        u <- M.read uv i
        let x = d ^-^ c*^x'
            w = u - c*w'
        M.write dv i x
        M.write uv i w
        backward (i-1) x w
  xn <- M.read dv n
  wn <- M.read uv n
  backward (n-1) xn wn

  x0 <- M.read dv 0
  w0 <- M.read uv 0

  let dsum = x0 ^+^ xn
      usum = w0 + wn
      !m = dsum ^/ (1 + usum)

  for_ [0..n] $ \i -> do
    x <- M.read dv i
    w <- M.read uv i
    let y = x ^-^ w *^ m
    M.write dv i y

  pure dv

{-# SPECIALISE
  cubicSplineLoopTangents :: U.Vector (V2 Double) -> V2 Double -> U.Vector (V2 Double)
  #-}

-- $cyclic-derivation
--
-- For the cyclic case the matrix form of the equations is now
--
--      | 4 1       1 |
--      | 1 4 1       |
--      |   1 4 1     |
--  C = |     ...     |
--      |       1 4 1 |
--      | 1       1 4 |
--
-- This matrix cannot be solved directly using the Thomas algorithm.
-- Instead we make use of the Sherman-Morrison formula which states
--
--   (A + uv^T)^-1 = A^-1 - (A^-1 uv^T A^-1)/(1 + v^T A^-1 u)
--
-- for an invertable matrix A and column vectors u and v iff
-- 1 + v^T A^-1 u /= 0.
--
-- We choose column vectors
--
--   u = v = [1, 0, ..., 0, 1]^T
--
-- so that C = A + uv^T where
--
--      | 3 1         |
--      | 1 4 1       |
--      |   1 4 1     |
--  A = |     ...     |
--      |       1 4 1 |
--      |         1 3 |
--
-- Now remember we wish to solve for x in the equation
--
-- Cx = w
--
-- where w is the vector containing sums of the offsets we're making the
-- cubic spline out of. To solve we multiply by C^-1 on the left:
--
-- x = C^-1 w
--   = (A + uv^T)^-1 x
--
-- Now we make use of the Sherman-Morrison formula
--
-- x = (A^-1 - (A^-1 uv^T A^-1)/(1 + v^T A^-1 u)) w
--   = A^-1 w - (A^-1 uv^T A^-1 w)/(1 + v^T A^-1 u)
--
-- If we look carefully A^-1 is multiplied the two columns vectors w and
-- u in this equation. Denoting
--
--   w' = A^-1 w
--   u' = A^-1 u
--
-- we have
--
-- x = w' - (u' v^T w')/(1 + v^T u')
--
-- Let
--
--   w_sum = v^T w'
--         = w'[0] + w'[n]
--
-- and
--
--   u_sum = v^T u'
--         = u'[0] + u'[n]
--
-- Now v^T w' and 1 + v^T u' are scalars so we can write them as a
-- coefficient to u':
--
-- x = w' - (v^T w')/(1 + v^T u') u'
--   = w' - w_sum/(1 + u_sum) u'
--
-- Remember A is a tridiagonal matrix so we solve
--
--   Aw' = w
--   Au' = u
--
-- using the Thomas algorithm (in one pass) to solve and use the
-- equation for x above to get the values we need.

