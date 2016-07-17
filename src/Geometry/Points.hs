{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Points
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A type for /points/ (as distinct from vectors).
--
-----------------------------------------------------------------------------

module Geometry.Points
  ( -- * Points
    Point (..)
  , Affine (..)
  , origin
  , (*.)
  , relative
  , _Point
  , centroid

  , reflectThrough
  , mirror
  , relative2
  , relative3
  ) where

import           Control.Lens    (over)
import qualified Data.Foldable as F

import           Linear.Affine
import           Linear.Vector

import           Geometry.Space

type instance V (Point v n) = v
type instance N (Point v n) = n

mirror :: (Additive v, Num n) => Point v n -> Point v n
mirror = reflectThrough origin

-- | Scale a point by a scalar. Specialized version of '(*^)'.
(*.) :: (Functor v, Num n) => n -> Point v n -> Point v n
(*.) = (*^)

-- | Apply a transformation relative to the given point.
relative2 :: (Additive v, Num n)
  => Point v n -> (v n -> v n -> v n)
  -> Point v n -> Point v n -> Point v n
relative2 p f x y = (p .+^) $ f (inj x) (inj y) where inj = (.-. p)

-- | Apply a transformation relative to the given point.
relative3 :: (Additive v, Num n)
  => Point v n -> (v n -> v n -> v n -> v n)
  -> Point v n -> Point v n -> Point v n -> Point v n
relative3 p f x y z = (p .+^) $ f (inj x) (inj y) (inj z) where inj = (.-. p)

-- | Mirror a point through a given point.
reflectThrough :: (Additive v, Num n) => Point v n -> Point v n -> Point v n
reflectThrough o = over (relative o) negated

-- | The centroid of a set of /n/ points is their sum divided by /n/.
centroid :: (Foldable f, Additive v, Fractional n) => f (Point v n) -> Point v n
centroid = uncurry (^/) . F.foldl' (\(s,c) e -> (e ^+^ s,c+1)) (zero,0)
{-# INLINE centroid #-}

