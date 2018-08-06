-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.ThreeD.Vector
-- Copyright   :  (c) 2013-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Three-dimensional vectors.
--
-----------------------------------------------------------------------------
module Geometry.ThreeD.Vector
  ( -- * Special 3D vectors
    unitX, unitY, unitZ, unit_X, unit_Y, unit_Z
  , xDir, yDir, zDir, x_Dir, y_Dir, z_Dir
  ) where

import           Control.Lens          ((&), (.~))

import           Geometry.Direction
import           Geometry.ThreeD.Types
import           Geometry.TwoD.Vector

import           Linear.Vector

-- | The unit vector in the positive Z direction.
unitZ :: (R3 v, Additive v, Num n) => v n
unitZ = zero & _z .~ 1

-- | The unit vector in the negative Z direction.
unit_Z :: (R3 v, Additive v, Num n) => v n
unit_Z = zero & _z .~ (-1)

-- | A 'Direction' pointing in the positive Z direction.
zDir :: (R3 v, Additive v, Num n) => Direction v n
zDir = Dir unitZ

-- | A 'Direction' pointing in the negative Z direction.
z_Dir :: (R3 v, Additive v, Num n) => Direction v n
z_Dir = Dir unit_Z
