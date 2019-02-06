-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.TwoD.Vector
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Two-dimensional vectors.
--
-----------------------------------------------------------------------------
module Geometry.TwoD.Vector
  ( -- * Special 2D vectors
    unitX, unitY, unit_X, unit_Y
  , xDir, yDir, x_Dir, y_Dir

    -- * Converting between vectors and angles
  , angleV, angleDir, e, signedAngleBetween, signedAngleBetweenDirs

    -- * 2D vector utilities
  , perp, leftTurn, crossZ

  ) where

import           Control.Lens        (view, (&), (.~), (^.))

import           Geometry.Angle
import           Geometry.Direction
import           Geometry.TwoD.Types

import           Linear.Metric
import           Linear.V2
import           Linear.Vector

-- | The unit vector in the positive X direction.
unitX :: (R1 v, Additive v, Num n) => v n
unitX = zero & _x .~ 1

-- | The unit vector in the negative X direction.
unit_X :: (R1 v, Additive v, Num n) => v n
unit_X = zero & _x .~ (-1)

-- | The unit vector in the positive Y direction.
unitY :: (R2 v, Additive v, Num n) => v n
unitY = zero & _y .~ 1

-- | The unit vector in the negative Y direction.
unit_Y :: (R2 v, Additive v, Num n) => v n
unit_Y = zero & _y .~ (-1)

-- | A 'Direction' pointing in the positive X direction.
xDir :: (R1 v, Additive v, Num n) => Direction v n
xDir = Dir unitX

-- | A 'Direction' pointing in the positive Y direction.
yDir :: (R2 v, Additive v, Num n) => Direction v n
yDir = Dir unitY

-- | A 'Direction' pointing in the negative X direction.
x_Dir :: (R1 v, Additive v, Num n) => Direction v n
x_Dir = Dir unit_X

-- | A 'Direction' pointing in the negative Y direction.
y_Dir :: (R2 v, Additive v, Num n) => Direction v n
y_Dir = Dir unit_Y

-- | A direction at a specified angle counterclockwise from the 'xDir'.
angleDir :: Floating n => Angle n -> Direction V2 n
angleDir = Dir . angleV

-- | A unit vector at a specified angle counterclockwise from the
--   positive X axis (see also 'e').
angleV :: Floating n => Angle n -> V2 n
angleV = angle . view rad

-- | A unit vector at a specified angle counterclockwise from the
--   positive X axis. 'e' is a synonym for 'angleV', but provided as a
--   sort of pun: @r *^ e theta@ can be used to construct a vector of
--   length @r@ in the direction @theta@, just as \(r e^{i \theta}\)
--   constructs a corresponding complex number.
e :: Floating n => Angle n -> V2 n
e = angleV

-- | @leftTurn v1 v2@ tests whether the direction of @v2@ is a left
--   turn from @v1@ (that is, if the direction of @v2@ can be obtained
--   from that of @v1@ by adding an angle 0 <= theta <= tau/2).
leftTurn :: (Num n, Ord n) => V2 n -> V2 n -> Bool
leftTurn v1 v2 = (v1 `dot` perp v2) < 0

-- | Signed angle between two vectors. Currently defined as
--
-- @
-- signedAngleBetween u v = (u ^. _theta) ^-^ (v ^. _theta)
-- @
signedAngleBetween :: RealFloat n => V2 n -> V2 n -> Angle n
signedAngleBetween u v = (u ^. _theta) ^-^ (v ^. _theta)
-- do we need to use _theta here?

-- | Same as 'signedAngleBetween' but for 'Directions's.
signedAngleBetweenDirs :: RealFloat n => Direction V2 n -> Direction V2 n -> Angle n
signedAngleBetweenDirs u v = (u ^. _theta) ^-^ (v ^. _theta)

