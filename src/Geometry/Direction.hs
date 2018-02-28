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
-- A type for representing /directions/, which can be thought of as
-- vectors whose magnitude has been forgotten, along with various
-- utility functions.  The 'Direction' type is polymorphic over the
-- vector space.
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
import           Data.Functor.Classes

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
--   If the constructor 'Dir' is used, the vector /must/ be a unit
--   vector.
newtype Direction v n = Dir (v n)
  deriving Functor

instance (Eq1 v, Eq n) => Eq (Direction v n) where
  Dir v1 == Dir v2 = eq1 v1 v2
  {-# INLINE (==) #-}

instance Show1 v => Show1 (Direction v) where
  liftShowsPrec x y d (Dir v) = showParen (d > 10) $
    showString "direction " . liftShowsPrec x y 11 v

instance (Show1 v, Show n) => Show (Direction v n) where
  showsPrec = showsPrec1

type instance V (Direction v n) = v
type instance N (Direction v n) = n

instance (Metric v, F.Foldable v, Floating n) => Transformable (Direction v n) where
  transform t = _Dir %~ signorm . apply t

instance HasTheta v => HasTheta (Direction v) where
  _theta = _Dir . _theta

instance HasPhi v => HasPhi (Direction v) where
  _phi = _Dir . _phi

-- | @_Dir@ is provided to allow efficient implementations of
--   functions in particular vector-spaces, but should be used with
--   care as it exposes too much information.  In particular it must
--   not be used to create a @Direction@ out of a non-unit vector.
_Dir :: Iso' (Direction v n) (v n)
_Dir = iso (\(Dir v) -> v) Dir
{-# INLINE _Dir #-}

-- | @direction v@ is the direction in which @v@ points.  Returns an
--   unspecified value when given the zero vector as input.
direction :: (Metric v, Floating n) => v n -> Direction v n
direction = Dir . signorm
{-# INLINE direction #-}

-- | A synonym for 'direction'.
dir :: (Metric v, Floating n) => v n -> Direction v n
dir = direction
{-# INLINE dir #-}

-- | @fromDirection d@ is the unit vector in the direction @d@.
fromDirection :: Direction v n -> v n
fromDirection (Dir v) = v
{-# INLINE fromDirection #-}

-- | A synonym for 'fromDirection'.
fromDir :: Direction v n -> v n
fromDir = fromDirection
{-# INLINE fromDir #-}

-- | Compute the positive angle between the two directions in their
--   common plane, returning an angle in the range $[0,\pi]$.  In
--   particular, note that @angleBetweenDirs@ is commutative.
angleBetweenDirs :: (Metric v, Floating n)
  => Direction v n -> Direction v n -> Angle n
angleBetweenDirs d1 d2 = angleBetween (fromDir d1) (fromDir d2)

-- | @dirBetween p q@ computes the direction from @p@ to @q@.
dirBetween :: (Metric v, Floating n) => Point v n -> Point v n -> Direction v n
dirBetween p q = dir $ q .-. p

