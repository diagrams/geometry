{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.TwoD.Ellipse
-- Copyright   :  (c) 2011-2016 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Two-dimensional ellipses (and, as a special case, circles).
--
-----------------------------------------------------------------------------

module Geometry.TwoD.Ellipse
    (
      -- * Ellipse and circle diagrams
      unitCircle
    , circle
    , ellipse
    , ellipseXY
    ) where

import           Geometry.Space

import           Geometry.Angle
import           Geometry.Located        (at)
import           Geometry.Trail          (glueTrail)
import           Geometry.TrailLike
import           Geometry.TwoD.Arc
import           Geometry.Transform
import           Geometry.TwoD.Transform
import           Geometry.TwoD.Types
import           Geometry.TwoD.Vector    (xDir)

-- | A circle of radius 1, with center at the origin.
unitCircle :: (InSpace V2 n t, TrailLike t) => t
unitCircle = trailLike $ glueTrail (arcT xDir fullTurn) `at` p2 (1,0)

-- | A circle of the given radius, centered at the origin.  As a path,
--   it begins at (r,0).
circle :: (InSpace V2 n t, TrailLike t) => n -> t
circle d = trailLike $ scale d unitCircle

-- | @ellipse e@ constructs an ellipse with eccentricity @e@ by
--   scaling the unit circle in the X direction.  The eccentricity must
--   be within the interval [0,1).
ellipse :: (InSpace V2 n t, TrailLike t) => n -> t
ellipse e
    | e >= 0 && e < 1  = trailLike $ scaleX (sqrt (1 - e*e)) unitCircle
    | otherwise        = error "Eccentricity of ellipse must be >= 0 and < 1."

-- | @ellipseXY x y@ creates an axis-aligned ellipse, centered at the
--   origin, with radius @x@ along the x-axis and radius @y@ along the
--   y-axis.
ellipseXY :: (InSpace V2 n t, TrailLike t) => n -> n -> t
ellipseXY x y = trailLike $ scaleV (V2 x y) unitCircle
