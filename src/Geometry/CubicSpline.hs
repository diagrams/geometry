{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
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
    -- * Constructing paths from cubic splines
    cubicSplineLine
  , cubicSplineLoop
  , bspline
  ) where

-- import           Control.Lens                  (view)

-- import           Geometry.Space
import           Geometry.CubicSpline.Boehm
import           Geometry.CubicSpline.Internal
-- import           Geometry.Located              (Located, at, mapLoc)
import           Geometry.Segment
import           Geometry.Trail
-- import           Geometry.TrailLike            (TrailLike (..))

import           Linear.Affine
import           Linear.Vector

import qualified Data.Vector as B

-- | Construct a spline path-like thing of cubic segments from a list of
--   vertices, with the first vertex as the starting point.  The first
--   argument specifies whether the path should be closed.
--
--   <<diagrams/src_Diagrams_CubicSpline_cubicSplineEx.svg#diagram=cubicSplineEx&width=600>>
--
--   > pts = map p2 [(0,0), (2,3), (5,-2), (-4,1), (0,3)]
--   > spot = circle 0.2 # fc blue # lw none
--   > mkPath closed = position (zip pts (repeat spot))
--   >              <> cubicSpline closed pts
--   > cubicSplineEx = (mkPath False ||| strutX 2 ||| mkPath True)
--   >               # centerXY # pad 1.1
--
--   For more information, see <http://mathworld.wolfram.com/CubicSpline.html>.
-- cubicSpline :: (InSpace v n t, FromTrail t, Fractional (v n)) => Bool -> [Point v n] -> t
-- cubicSpline closed []  = fromLocTrail . closeIf closed $ mempty `at` origin
-- cubicSpline closed [p] = fromLocTrail . closeIf closed $ mempty `at` p
-- cubicSpline closed ps  = flattenBeziers . map f . solveCubicSplineCoefficients closed . map (view lensP) $ ps
--   where
--     f [a,b,c,d] = [a, (3*a+b)/3, (3*a+2*b+c)/3, a+b+c+d]
--     flattenBeziers bs@((b:_):_)
--       = fromLocTrail . closeIf closed $ lineFromSegments (map bez bs) `at` P b
--     bez [a,b,c,d] = bezier3 (b - a) (c - a) (d - a)


-- closeIf :: (Additive v, Num n)
--         => Bool -> Located (Line v n) -> Located (Trail v n)
-- closeIf c = mapLoc (if c then wrapLoop . glueLine else wrapLine)

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

-- cubic
-- spline @c@ for offsets @vs@ has the following properties:
--
--   - the offsets of the cubic segments match the input offsets
--   - the tangent at end of segment matches the tangent at the begining
--     of the next segment (smooth)
--   -
--
-- @
-- (c ^.. segments . to offset) == vs
-- tangent
--


cubicSplineLine
  :: (Additive v, Fractional n)
  => [v n] -> Line v n
cubicSplineLine vs
  | n <= 1    = lineFromSegments $ map Linear vs
  | otherwise = cubicSplineLineFromTangents vv off dv
  where
  vv  = B.fromList vs
  n   = B.length vv
  lst = B.foldl' (.+^) origin vv
  off = origin .-. lst
  dv  = cubicSplineLineTangents vv
{-# INLINE [0] cubicSplineLine #-}

cubicSplineLoop
  :: (Additive v, Fractional n)
  => [v n] -> Loop v n
cubicSplineLoop vs
  | n <= 1    = loopFromSegments (map Linear vs) linearClosing
  | otherwise = cubicSplineLoopFromTangents vv off dv
  where
  vv  = B.fromList vs
  n   = B.length vv
  lst = B.foldl' (.+^) origin vv
  off = origin .-. lst
  dv  = cubicSplineLoopTangents vv off
{-# INLINE [0] cubicSplineLoop #-}

-- {-# RULES
--  "cubicSpline/V2 Double" cubicSplineLine = cubicSplineLineV2D
--  #-}

-- cubicSplineLineV2D :: Foldable f => f (V2 Double) -> Line V2 Double
-- cubicSplineLineV2D = cubicSplineLineVec . U.fromList . toList
-- {-# INLINE cubicSplineLineV2D #-}
