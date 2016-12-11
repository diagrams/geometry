{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Angle
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Type for representing angles.
--
-----------------------------------------------------------------------------

module Geometry.Angle
  ( -- * Angle type
    Angle (..)

    -- ** Using angles
  , (@@), rad, turn, deg

    -- ** Common angles
  , fullTurn, halfTurn, quarterTurn

    -- ** Trigonometric functions
  , sinA, cosA, tanA
  , asinA, acosA, atanA, atan2A, atan2A'

    -- ** Angle utilities
  , angleBetween, angleRatio, normalizeAngle

    -- ** Classes
  , HasTheta(..)
  , HasPhi(..)
  ) where

import           Control.Applicative
import           Control.Lens        (AReview, Iso', Lens', iso, over, review, (^.))
import           Data.Fixed
import           Data.Monoid         hiding ((<>))
import           Data.Semigroup
import           Text.Read
import           Prelude

import           Geometry.Space
import           Geometry.Points

import           Linear.Metric
import           Linear.Vector

-- | Angles can be expressed in a variety of units.  Internally,
--   they are represented in radians.
newtype Angle n = Radians n
  deriving (Eq, Ord, Enum, Functor)

instance Show n => Show (Angle n) where
  showsPrec d (Radians a) = showParen (d > 5) $
    showsPrec 6 a . showString " @@ rad"

instance Read n => Read (Angle n) where
  readPrec = parens . prec 5 $ do
    x <- readPrec
    Symbol "@@" <- lexP
    Ident "rad" <- lexP
    pure (Radians x)

type instance N (Angle n) = n

instance Applicative Angle where
  pure = Radians
  {-# INLINE pure #-}
  Radians f <*> Radians x = Radians (f x)
  {-# INLINE (<*>) #-}

instance Additive Angle where
  zero = pure 0
  {-# INLINE zero #-}

instance Num n => Semigroup (Angle n) where
  (<>) = (^+^)
  {-# INLINE (<>) #-}

instance Num n => Monoid (Angle n) where
  mappend = (<>)
  mempty  = Radians 0

-- | The radian measure of an 'Angle' @a@ can be accessed as @a '^.'
--   rad@. A new 'Angle' can be defined in radians as @pi \@\@
--   rad@.
rad :: Iso' (Angle n) n
rad = iso (\(Radians r) -> r) Radians
{-# INLINE rad #-}

-- | The measure of an 'Angle' @a@ in full circles can be accessed as
--   @a '^.' turn@.  A new 'Angle' of one-half circle can be defined in as
--   @1/2 \@\@ turn@.
turn :: Floating n => Iso' (Angle n) n
turn = iso (\(Radians r) -> r / (2*pi)) (Radians . (*(2*pi)))
{-# INLINE turn #-}

-- | The degree measure of an 'Angle' @a@ can be accessed as @a
--   '^.' deg@. A new 'Angle' can be defined in degrees as @180 \@\@
--   deg@.
deg :: Floating n => Iso' (Angle n) n
deg = iso (\(Radians r) -> r / (2*pi/360)) (Radians . ( * (2*pi/360)))
{-# INLINE deg #-}

-- | An angle representing one full turn.
fullTurn :: Floating v => Angle v
fullTurn = 1 @@ turn

-- | An angle representing a half turn.
halfTurn :: Floating v => Angle v
halfTurn = 0.5 @@ turn

-- | An angle representing a quarter turn.
quarterTurn :: Floating v => Angle v
quarterTurn = 0.25 @@ turn

-- | Calculate ratio between two angles.
angleRatio :: Floating n => Angle n -> Angle n -> n
angleRatio a b = (a ^. rad) / (b ^. rad)

-- | The sine of the given @Angle@.
sinA :: Floating n => Angle n -> n
sinA (Radians r) = sin r

-- | The cosine of the given @Angle@.
cosA :: Floating n => Angle n -> n
cosA (Radians r) = cos r

-- | The tangent function of the given @Angle@.
tanA :: Floating n => Angle n -> n
tanA (Radians r) = tan r

-- | The @Angle@ with the given sine.
asinA :: Floating n => n -> Angle n
asinA = Radians . asin

-- | The @Angle@ with the given cosine.
acosA :: Floating n => n -> Angle n
acosA = Radians . acos

-- | The @Angle@ with the given tangent.
atanA :: Floating n => n -> Angle n
atanA = Radians . atan

-- | @atan2A y x@ is the angle between the positive x-axis and the vector given
--   by the coordinates (x, y). The 'Angle' returned is in the [-pi,pi] range.
atan2A :: RealFloat n => n -> n -> Angle n
atan2A y x = Radians $ atan2 y x

-- | Similar to 'atan2A' but without the 'RealFloat' constraint. This means it
--   doesn't handle negative zero cases. However, for most geometric purposes,
--   the outcome will be the same.
atan2A' :: OrderedField n => n -> n -> Angle n
atan2A' y x = atan2' y x @@ rad
{-# INLINE atan2A' #-}

-- atan2 without negative zero tests
atan2' :: OrderedField n => n -> n -> n
atan2' y x
  | x > 0            =  atan (y/x)
  | x == 0 && y > 0  =  pi/2
  | x <  0 && y > 0  =  pi + atan (y/x)
  | x <= 0 && y < 0  = -atan2' (-y) x
  | y == 0 && x < 0  =  pi    -- must be after the previous test on zero y
  | x==0 && y==0     =  y     -- must be after the other double zero tests
  | otherwise        =  x + y -- x or y is a NaN, return a NaN (via +)
{-# INLINE atan2' #-}


-- | @30 \@\@ deg@ is an 'Angle' of the given measure and units.
--
-- >>> pi @@ rad
-- 3.141592653589793 @@ rad
--
-- >>> 1 @@ turn
-- 6.283185307179586 @@ rad
--
-- >>> 30 @@ deg
-- 0.5235987755982988 @@ rad
--
--   For 'Iso''s, ('@@') reverses the 'Iso'' on its right, and applies
--   the 'Iso'' to the value on the left. 'Angle's are the motivating
--   example where this order improves readability.
--
--   This is the same as a flipped 'review'.
--
-- @
-- ('@@') :: a -> 'Iso''      s a -> s
-- ('@@') :: a -> 'Prism''    s a -> s
-- ('@@') :: a -> 'Review'    s a -> s
-- ('@@') :: a -> 'Equality'' s a -> s
-- @
(@@) :: b -> AReview a b -> a
a @@ i = review i a

infixl 5 @@

-- | Compute the positive angle between the two vectors in their common
--   plane in the [0,pi] range. For a signed angle see
--   'Diagrams.TwoD.Vector.signedAngleBetween'.
--
--   Returns NaN if either of the vectors are zero.
angleBetween  :: (Metric v, Floating n) => v n -> v n -> Angle n
angleBetween v1 v2 = acosA (signorm v1 `dot` signorm v2)
-- N.B.: Currently discards the common plane information.

-- | Normalize an angle so that it lies in the [0,tau) range.
normalizeAngle :: (Floating n, Real n) => Angle n -> Angle n
normalizeAngle = over rad (`mod'` (2 * pi))

------------------------------------------------------------
-- Polar Coordinates

-- | The class of types with at least one angle coordinate, called '_theta'.
class HasTheta t where
  _theta :: RealFloat n => Lens' (t n) (Angle n)

-- | The class of types with at least two angle coordinates, the second called
--   '_phi'. '_phi' is the positive angle measured from the z axis.
class HasTheta t => HasPhi t where
  _phi :: RealFloat n => Lens' (t n) (Angle n)

-- Point instances
instance HasTheta v => HasTheta (Point v) where
  _theta = _Point . _theta
  {-# INLINE _theta #-}

instance HasPhi v => HasPhi (Point v) where
  _phi = _Point . _phi
  {-# INLINE _phi #-}
