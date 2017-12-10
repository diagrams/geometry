{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Direction
-- Copyright   :  (c) 2014-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Type for representing directions, polymorphic in vector space
--
-----------------------------------------------------------------------------

module Geometry.Direction
  ( Direction (..)
  , _Dir
  , direction, dir
  , fromDirection, fromDir
  , angleBetweenDirs
  , dirBetween
  ) where

import           Control.Lens   (Iso', iso, (%~))
import           Data.Foldable as F

import           Geometry.Angle
import           Geometry.Space
import           Geometry.Transform

import           Linear.Affine
import           Linear.Metric

--------------------------------------------------------------------------------
-- Direction

-- | A vector is described by a @Direction@ and a magnitude.  So we
--   can think of a @Direction@ as a vector that has forgotten its
--   magnitude.  @Direction@s can be used with 'fromDirection' and the
--   lenses provided by its instances.
--
--   If the constructor is used, the vector /must/ be a unit vector.
newtype Direction v n = Dir (v n)
  deriving (Read, Show, Eq, Ord, Functor) -- todo: special instances

type instance V (Direction v n) = v
type instance N (Direction v n) = n

instance (Metric v, F.Foldable v, Floating n) => Transformable (Direction v n) where
  transform t = _Dir %~ signorm . apply t

instance HasTheta v => HasTheta (Direction v) where
  _theta = _Dir . _theta

instance HasPhi v => HasPhi (Direction v) where
  _phi = _Dir . _phi

-- | _Dir is provided to allow efficient implementations of functions
--   in particular vector-spaces, but should be used with care as it
--   exposes too much information.
_Dir :: Iso' (Direction v n) (v n)
_Dir = iso (\(Dir v) -> v) Dir
{-# INLINE _Dir #-}

-- | @direction v@ is the direction in which @v@ points.  Returns an
--   unspecified value when given the zero vector as input.
direction :: (Metric v, Floating n) => v n -> Direction v n
direction = Dir . signorm
{-# INLINE direction #-}

-- | Synonym for 'direction'.
dir :: (Metric v, Floating n) => v n -> Direction v n
dir = direction
{-# INLINE dir #-}

-- | @fromDirection d@ is the unit vector in the direction @d@.
fromDirection :: Direction v n -> v n
fromDirection (Dir v) = v
{-# INLINE fromDirection #-}

-- | Synonym for 'fromDirection'.
fromDir :: Direction v n -> v n
fromDir = fromDirection
{-# INLINE fromDir #-}

-- | compute the positive angle between the two directions in their common plane
angleBetweenDirs :: (Metric v, Floating n)
  => Direction v n -> Direction v n -> Angle n
angleBetweenDirs d1 d2 = angleBetween (fromDir d1) (fromDir d2)

-- | @dirBetween p q@ returns the directions from @p@ to @q@
dirBetween :: (Metric v, Floating n) => Point v n -> Point v n -> Direction v n
dirBetween p q = dir $ p .-. q

