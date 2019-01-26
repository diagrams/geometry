{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.CubicSpline
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A /cubic spline/ is a smooth, connected sequence of cubic curves.
-- This module provides two methods for constructing splines.
--
-- The 'cubicSpline' method can be used to create closed or open cubic
-- splines from a list of points. The resulting splines /pass through/
-- all the control points, but depend on the control points in a
-- "global" way (that is, changing one control point may alter the
-- entire curve).  For access to the internals of the spline
-- generation algorithm, see "Diagrams.CubicSpline.Internal".
--
-- 'bspline' creates a cubic B-spline, which starts and ends at the
-- first and last control points, but does not necessarily pass
-- through any of the other control points.  It depends on the control
-- points in a "local" way, that is, changing one control point will
-- only affect a local portion of the curve near that control point.
--
-----------------------------------------------------------------------------
module Geometry.CubicSpline
  (
    -- * Cubic splines
    cubicSpline
  , cubicSplineLine
  , cubicSplineLoop
  , cubicSplineLineVec
  , cubicSplineLoopVec

    -- * B-splines
  , bspline

  ) where


import           Geometry.CubicSpline.Boehm
import           Geometry.CubicSpline.Internal
import           Geometry.Located
import           Geometry.Segment
import           Geometry.Space
import           Geometry.Trail

import           Linear
import           Linear.Affine

import qualified Data.Vector                   as B
import qualified Data.Vector.Generic           as V
import qualified Data.Vector.Unboxed           as U

-- | Construct a spline path-like thing of cubic segments from a list of
--   vertices, with the first vertex as the starting point.  The first
--   argument specifies whether the path should be closed.
--
--   <<diagrams/src_Geometry_CubicSpline_cubicSplineEx.svg#diagram=cubicSplineEx&width=600>>
--
--   > import Geometry.CubicSpline
--   > pts = map p2 [(0,0), (2,3), (5,-2), (-4,1), (0,3)]
--   > spot = circle 0.2 # fc blue # lw none
--   > mkPath closed = position (zip pts (repeat spot))
--   >              <> cubicSpline closed pts
--   > cubicSplineEx = (mkPath False ||| strutX 2 ||| mkPath True)
--   >               # centerXY # pad 1.1
--
--   For more information, see <http://mathworld.wolfram.com/CubicSpline.html>.
cubicSpline
  :: (InSpace v n t, FromTrail t, Additive v, Fractional n)
  => Bool -> [Point v n] -> t
cubicSpline _ [] = fromLocTrail $ mempty `at` origin
cubicSpline closed pps@(p:ps)
  | closed    = fromLocLoop $ cubicSplineLoop offsets `at` p
  | otherwise = fromLocLine $ cubicSplineLine offsets `at` p
  where offsets = zipWith (flip (.-.)) pps ps

-- $cubic-spline
-- A cubic spline is a smooth curve made up of cubic bezier segments
-- whose offsets match the input offsets.
--
--   - For lines the curvatures at the start of the first segment and
--     end of the last segment are both zero (a "natural" cubic spline).
--   - For loops the tangent at the end of last segment matches the
--     tangent at the begining of the first segment.
--
-- These requirements uniquely define the cubic spline. In the case that
-- only one offset is given, a linear segment is returned.

-- Lines ---------------------------------------------------------------

-- | See 'cubicSpline'.
cubicSplineLineVec
  :: (V.Vector vec (v n), V.Vector vec n, Additive v, Fractional n)
  => vec (v n)
  -> Line v n
cubicSplineLineVec vs
  | n <= 1    = lineFromSegments $ map Linear (V.toList vs)
  | otherwise = cubicSplineLineFromTangents vs off dv
  where
  n   = V.length vs
  off = V.foldl' (^+^) zero vs
  dv  = cubicSplineLineTangents vs
{-# INLINE cubicSplineLineVec #-}

cubicSplineLineV2D
  :: [V2 Double]
  -> Line V2 Double
cubicSplineLineV2D = cubicSplineLineVec . U.fromList

-- | See 'cubicSpline'.
cubicSplineLine
  :: (Additive v, Fractional n)
  => [v n] -> Line v n
cubicSplineLine = cubicSplineLineVec . B.fromList
{-# INLINE [0] cubicSplineLine #-}

-- Loops ---------------------------------------------------------------

cubicSplineLoopVec
  :: (V.Vector vec (v n), V.Vector vec n, Additive v, Fractional n)
  => vec (v n) -> Loop v n
cubicSplineLoopVec vs
  | n <= 1    = loopFromSegments (map Linear (V.toList vs)) linearClosing
  | otherwise = cubicSplineLoopFromTangents vs off dv
  where
  n   = V.length vs
  off = V.foldl' (^+^) zero vs
  dv  = cubicSplineLoopTangents vs (negated off)
{-# INLINE cubicSplineLoopVec #-}

-- | See 'cubicSpline'.
cubicSplineLoopV2D
  :: [V2 Double] -> Loop V2 Double
cubicSplineLoopV2D = cubicSplineLoopVec . U.fromList

-- | See 'cubicSpline'.
cubicSplineLoop
  :: (Additive v, Fractional n)
  => [v n] -> Loop v n
cubicSplineLoop = cubicSplineLoopVec . B.fromList
{-# INLINE [0] cubicSplineLoop #-}

{-# RULES
 "cubicSplineLine/V2 Double" cubicSplineLine = cubicSplineLineV2D;
 "cubicSplineLoop/V2 Double" cubicSplineLoop = cubicSplineLoopV2D
 #-}

