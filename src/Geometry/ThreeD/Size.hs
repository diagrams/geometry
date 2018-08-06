{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.ThreeD.Size
-- Copyright   :  (c) 2014-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Utilities for working with sizes of three-dimensional objects.
--
-----------------------------------------------------------------------------
module Geometry.ThreeD.Size
  (
    -- * Computing sizes
    extentX, extentY, extentZ

    -- * Specifying sizes
  , mkSizeSpec3D
  , dims3D

  ) where

import           Geometry.Envelope
import           Geometry.Size
import           Geometry.Space
import           Geometry.ThreeD.Types
import           Geometry.ThreeD.Vector
import           Geometry.TwoD.Size

------------------------------------------------------------
-- Computing geometry sizes
------------------------------------------------------------

-- | Compute the absolute z-coordinate range of an enveloped object in
--   the form @(lo,hi)@. Return @Nothing@ for objects with an empty
--   envelope.
extentZ :: (InSpace v n a, R3 v, Enveloped a) => a -> Maybe (n, n)
extentZ = extent unitZ

-- | Make a 3D 'SizeSpec' from possibly-specified width, height, and depth.
mkSizeSpec3D :: Num n => Maybe n -> Maybe n -> Maybe n -> SizeSpec V3 n
mkSizeSpec3D x y z = mkSizeSpec (V3 x y z)

-- | Make a 3D 'SizeSpec' from a width, height, and depth.
dims3D :: n -> n -> n -> SizeSpec V3 n
dims3D x y z = dims (V3 x y z)

