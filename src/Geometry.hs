-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module reexports the geometry library. Constructors that are
-- not safe to use directly are not reexported from here but the
-- constructors are available by importing the module they are defined
-- in.
--
-----------------------------------------------------------------------------

module Geometry
  ( module Geometry.Angle
  , module Geometry.BoundingBox
  , module Geometry.Combinators
  , module Geometry.CubicSpline
  , module Geometry.Direction
  , module Geometry.Envelope
  , module Geometry.HasOrigin
  , module Geometry.Juxtapose
  , module Geometry.Located
  , module Geometry.Parametric
  , module Geometry.Path
  , module Geometry.Points
  , module Geometry.Query
  , module Geometry.Segment
  , module Geometry.Size
  , module Geometry.Space
  , module Geometry.Trace
  , module Geometry.Trail
  , module Geometry.Transform

    -- * TwoD
  , module Geometry.TwoD.Arc
  , module Geometry.TwoD.Combinators
  , module Geometry.TwoD.Curvature
  , module Geometry.TwoD.Ellipse
  , module Geometry.TwoD.Path
  , module Geometry.TwoD.Points
  , module Geometry.TwoD.Polygons
  , module Geometry.TwoD.Segment
  , module Geometry.TwoD.Size
  , module Geometry.TwoD.Shapes
  , module Geometry.TwoD.Transform
  , module Geometry.TwoD.Types
  , module Geometry.TwoD.Vector

    -- * ThreeD
  , module Geometry.ThreeD.Camera
  , module Geometry.ThreeD.Combinators
  , module Geometry.ThreeD.Size
  , module Geometry.ThreeD.Shapes
  , module Geometry.ThreeD.Transform
  , module Geometry.ThreeD.Vector
  , module Geometry.ThreeD.Types

  ) where

import           Geometry.Angle
import           Geometry.BoundingBox        hiding (BoundingBox (..))
import           Geometry.BoundingBox        (BoundingBox)
import           Geometry.Combinators
import           Geometry.CubicSpline
import           Geometry.Direction          hiding (Direction (..))
import           Geometry.Direction          (Direction)
import           Geometry.Envelope           hiding (Envelope (..))
import           Geometry.Envelope           (Envelope)
import           Geometry.HasOrigin
import           Geometry.Juxtapose
import           Geometry.Located
import           Geometry.Parametric
import           Geometry.Path               hiding (pathPoints)
import           Geometry.Points
import           Geometry.Query
import           Geometry.Segment
import           Geometry.Size               hiding (SizeSpec (..))
import           Geometry.Size               (SizeSpec)
import           Geometry.Space
import           Geometry.ThreeD.Camera
import           Geometry.ThreeD.Combinators
import           Geometry.ThreeD.Shapes
import           Geometry.ThreeD.Size
import           Geometry.ThreeD.Transform
import           Geometry.ThreeD.Types
import           Geometry.ThreeD.Vector
import           Geometry.Trace              hiding (Trace (..))
import           Geometry.Trace              (Trace)
import           Geometry.Trail              hiding (linePoints, loopPoints,
                                              trailPoints)
import           Geometry.Transform          hiding (Transformation (..))
import           Geometry.Transform          (Transformation)
import           Geometry.TwoD.Arc
import           Geometry.TwoD.Combinators
import           Geometry.TwoD.Curvature
import           Geometry.TwoD.Ellipse
import           Geometry.TwoD.Path
import           Geometry.TwoD.Points
import           Geometry.TwoD.Polygons
import           Geometry.TwoD.Segment
import           Geometry.TwoD.Shapes
import           Geometry.TwoD.Size
import           Geometry.TwoD.Transform
import           Geometry.TwoD.Types
import           Geometry.TwoD.Vector        hiding (e)
