{-# LANGUAGE CPP          #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Points
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A type for /points/ (as distinct from vectors).
--
-----------------------------------------------------------------------------

module Geometry.Points
  ( -- * Points
    Point (..)

  , P1
  , P2
  , P3
  , P4
  , pattern P1
  , pattern P2
  , pattern P3
  , pattern P4

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
import           Linear

import           Geometry.Space

type instance V (Point v n) = v
type instance N (Point v n) = n

type P1 = Point V1
type P2 = Point V2
type P3 = Point V3
type P4 = Point V4

pattern P1 :: a -> P1 a
pattern P1 x = P (V1 x)
pattern P2 :: a -> a -> P2 a
pattern P2 x y = P (V2 x y)
pattern P3 :: a -> a -> a -> P3 a
pattern P3 x y z = P (V3 x y z)
pattern P4 :: a -> a -> a -> a -> P4 a
pattern P4 x y z w = P (V4 x y z w)

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE P1 #-}
{-# COMPLETE P2 #-}
{-# COMPLETE P3 #-}
{-# COMPLETE P4 #-}
#endif

-- | Reflect a point through the origin.
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
