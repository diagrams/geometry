{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.TwoD.Transform
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Transformations specific to two dimensions, with a few generic
-- transformations (uniform scaling, translation) also re-exported for
-- convenience.
--
-----------------------------------------------------------------------------

module Geometry.TwoD.Transform
  (
    T2
    -- * Rotation
  , rotation, rotate, rotateBy, rotated
  , rotationAround, rotateAround
  , rotationTo, rotateTo

    -- * Scaling
  , scalingX, scaleX
  , scalingY, scaleY
  , scaling, scale

  , scaleToX, scaleToY
  , scaleUToX, scaleUToY

    -- * Translation
  , translationX, translateX
  , translationY, translateY
  , translation, translate

    -- * Reflection
  , reflectionX, reflectX
  , reflectionY, reflectY
  , reflectionXY, reflectXY
  , reflectionAbout, reflectAbout

    -- * Shears
  , shearingX, shearX
  , shearingY, shearY

  ) where

import qualified Data.Semigroup       as Sem
import           Geometry.Angle
import           Geometry.Direction
import           Geometry.Envelope
import           Geometry.Space
import           Geometry.Transform
import           Geometry.TwoD.Types
import           Geometry.TwoD.Vector

import           Control.Lens         hiding (at, transform)

import           Linear.Affine
import           Linear.Vector

-- Rotation ------------------------------------------------

-- | Create a transformation which performs a rotation about the local
--   origin by the given angle.  See also 'rotate'.
rotation :: Floating n => Angle n -> T2 n
rotation theta = fromOrthogonal (V2 (V2 c (-s)) (V2 s c))
  where
    c = cosA theta
    s = sinA theta
{-# INLINE rotation #-}

-- | Rotate about the local origin by the given angle. Positive angles
--   correspond to counterclockwise rotation, negative to
--   clockwise. The angle can be expressed using any of the 'Iso's on
--   'Angle'.  For example, @rotate (1\/4 \@\@ 'turn')@, @rotate
--   (tau\/4 \@\@ rad)@, and @rotate (90 \@\@ deg)@ all
--   represent the same transformation, namely, a counterclockwise
--   rotation by a right angle.  To rotate about some point other than
--   the local origin, see 'rotateAbout'.
--
--   Note that writing @rotate (1\/4)@, with no 'Angle' constructor,
--   will yield an error since GHC cannot figure out which sort of
--   angle you want to use.  In this common situation you can use
--   'rotateBy', which interprets its argument as a number of turns.

rotate :: (InSpace V2 n t, Transformable t, Floating n) => Angle n -> t -> t
rotate = transform . rotation
{-# INLINE rotate #-}

-- | A synonym for 'rotate', interpreting its argument in units of
-- turns; it can be more convenient to write @rotateBy (1\/4)@ than
-- @'rotate' (1\/4 \@\@ 'turn')@.
rotateBy :: (InSpace V2 n t, Transformable t, Floating n) => n -> t -> t
rotateBy = transform . rotation . review turn
{-# INLINE rotateBy #-}

-- | Use an 'Angle' to make an 'Iso' between an object
--   rotated and unrotated. This us useful for performing actions
--   'under' a rotation:
--
-- @
-- under (rotated t) f = rotate (negated t) . f . rotate t
-- rotated t ## a      = rotate t a
-- a ^. rotated t      = rotate (-t) a
-- over (rotated t) f  = rotate t . f . rotate (negated t)
-- @
rotated :: (InSpace V2 n a, Floating n, SameSpace a b, Transformable a, Transformable b)
        => Angle n -> Iso a b a b
rotated = transformed . rotation
{-# INLINE rotated #-}

-- | @rotationAbout p@ is a rotation about the point @p@ (instead of
--   around the local origin).
rotationAround :: Floating n => P2 n -> Angle n -> T2 n
rotationAround p theta =
  conjugate (translation (origin .-. p)) (rotation theta)
{-# INLINE rotationAround #-}

-- | @rotateAbout p@ is like 'rotate', except it rotates around the
--   point @p@ instead of around the local origin.
rotateAround :: (InSpace V2 n t, Transformable t, Floating n)
             => P2 n -> Angle n -> t -> t
rotateAround p theta = transform (rotationAround p theta)
{-# INLINE rotateAround #-}

-- | The rotation that aligns the x-axis with the given direction.
rotationTo :: OrderedField n => Direction V2 n -> T2 n
rotationTo (view _Dir -> V2 x y) = rotation (atan2A' y x)
{-# INLINE rotationTo #-}

-- | Rotate around the local origin such that the x axis aligns with the
--   given direction.
rotateTo :: (InSpace V2 n t, OrderedField n, Transformable t) => Direction V2 n -> t -> t
rotateTo = transform . rotationTo
{-# INLINE rotateTo #-}

-- Scaling -------------------------------------------------

-- | Construct a transformation which scales by the given factor in
--   the x (horizontal) direction.
scalingX :: (HasBasis v, R1 v, Fractional n) => n -> Transformation v n
scalingX c =
  fromLinear
    (eye & _x . _x .~ c)
    (eye & _x . _x //~ c)
{-# INLINE scalingX #-}

-- | Scale an object by the given factor in the x (horizontal)
--   direction.  To scale uniformly, use 'scale'.
scaleX :: (InSpace v n t, HasBasis v, R1 v, Fractional n, Transformable t) => n -> t -> t
scaleX = transform . scalingX
{-# INLINE scaleX #-}

-- | Construct a transformation which scales by the given factor in
--   the y (vertical) direction.
scalingY :: (HasBasis v, R2 v, Fractional n) => n -> Transformation v n
scalingY c =
  fromLinear
    (eye & _y . _y .~ c)
    (eye & _y . _y //~ c)
{-# INLINE scalingY #-}

-- | Scale an object by the given factor in the y (vertical)
--   direction.  To scale uniformly, use 'scale'.
scaleY :: (InSpace v n t, HasBasis v, R2 v, Fractional n, Transformable t)
  => n -> t -> t
scaleY = transform . scalingY
{-# INLINE scaleY #-}

-- | @scaleToX w@ scales an object in the x (horizontal) direction by
--   whatever factor required to make its width @w@.  @scaleToX@
--   should not be applied to objects with a width of 0, such as
--   'vrule'.
scaleToX :: (InSpace v n t, HasBasis v, R1 v, Enveloped t, Transformable t) => n -> t -> t
scaleToX w d = scaleX (w / diameter unitX d) d
{-# INLINE scaleToX #-}

-- | @scaleToY h@ scales an object in the y (vertical) direction by
--   whatever factor required to make its height @h@.  @scaleToY@
--   should not be applied to objects with a height of 0, such as
--   'hrule'.
scaleToY :: (InSpace v n t, HasBasis v, R2 v, Enveloped t, Transformable t) => n -> t -> t
scaleToY h d = scaleY (h / diameter unitY d) d
{-# INLINE scaleToY #-}

-- | @scaleUToX w@ scales an object /uniformly/ by whatever factor
--   required to make its width @w@.  @scaleUToX@ should not be
--   applied to objects with a width of 0, such as 'vrule'.
scaleUToX :: (InSpace v n t, HasBasis v, R1 v, Enveloped t, Transformable t) => n -> t -> t
scaleUToX w d = scale (w / diameter unitX d) d
{-# INLINE scaleUToX #-}

-- | @scaleUToY h@ scales an object /uniformly/ by whatever factor
--   required to make its height @h@.  @scaleUToY@ should not be applied
--   to objects with a height of 0, such as 'hrule'.
scaleUToY :: (InSpace v n t, HasBasis v, R2 v, Enveloped t, Transformable t) => n -> t -> t
scaleUToY h d = scale (h / diameter unitY d) d
{-# INLINE scaleUToY #-}

-- Translation ---------------------------------------------

-- | Construct a transformation which translates by the given distance
--   in the x (horizontal) direction.
translationX :: (HasBasis v, R1 v, Num n) => n -> Transformation v n
translationX x = translation (zero & _x .~ x)
{-# INLINE translationX #-}

-- | Translate an object by the given distance in the x (horizontal)
--   direction.
translateX :: (InSpace v n t, HasBasis v, R1 v, Transformable t) => n -> t -> t
translateX = transform . translationX
{-# INLINE translateX #-}

-- | Construct a transformation which translates by the given distance
--   in the y (vertical) direction.
translationY :: (HasBasis v, R2 v, Num n) => n -> Transformation v n
translationY y = translation (zero & _y .~ y)
{-# INLINE translationY #-}

-- | Translate an object by the given distance in the y (vertical)
--   direction.
translateY :: (InSpace v n t, HasBasis v, R2 v, Transformable t)
  => n -> t -> t
translateY = transform . translationY
{-# INLINE translateY #-}

-- Reflection ----------------------------------------------

-- | Construct a transformation which flips an object from left to
--   right, i.e. sends the point \((x,y)\) to \((-x,y)\).
reflectionX :: (HasBasis v, R1 v, Num n) => Transformation v n
reflectionX = fromInvoluted (eye & _x . _x .~ -1) -- (V2 (V2 (-1) 0) (V2 0 1))
{-# INLINE reflectionX #-}

-- | Flip an object from left to right, i.e. send the point \((x,y)\) to
--   \((-x,y)\).
reflectX :: (InSpace v n t, HasBasis v, R1 v, Transformable t) => t -> t
reflectX = transform reflectionX
{-# INLINE reflectX #-}

-- | Construct a transformation which flips an object from top to
--   bottom, i.e. sends the point \((x,y)\) to \((x,-y)\).
reflectionY :: (HasBasis v, R2 v, Num n) => Transformation v n
reflectionY = fromInvoluted (eye & _y . _y .~ -1) -- (V2 (V2 1 0) (V2 0 (-1)))
{-# INLINE reflectionY #-}

-- | Flip an object from top to bottom, i.e. send the point \((x,y)\) to
--   \((x,-y)\).
reflectY :: (InSpace v n t, HasBasis v, R2 v, Transformable t) => t -> t
reflectY = transform reflectionY
{-# INLINE reflectY #-}

-- | Construct a transformation which flips the object about the line \(x=y\), i.e.
--   sends the point \((x,y)\) to \((y,x)\).
reflectionXY :: Num n => Transformation V2 n
reflectionXY = fromInvoluted (V2 (V2 0 1) (V2 1 0))
{-# INLINE reflectionXY #-}

-- | Flips the object about x=y, i.e. send the point \((x,y)\) to \((y,x)\).
reflectXY :: (InSpace V2 n t, Transformable t) => t -> t
reflectXY = transform reflectionXY
{-# INLINE reflectXY #-}

-- | @reflectionAbout p d@ is a reflection in the line determined by
--   the point @p@ and direction @d@.
reflectionAbout :: OrderedField n => P2 n -> Direction V2 n -> T2 n
reflectionAbout p d =
  conjugate (rotationTo (reflectY d) Sem.<> translation (origin .-. p))
            reflectionY
{-# INLINE reflectionAbout #-}

-- | @reflectAbout p d@ reflects an object in the line determined by
--   the point @p@ and direction @d@.
reflectAbout :: (InSpace V2 n t, OrderedField n, Transformable t)
             => P2 n -> Direction V2 n -> t -> t
reflectAbout p v = transform (reflectionAbout p v)
{-# INLINE reflectAbout #-}

-- Shears --------------------------------------------------

-- | @shearingX d@ is the linear transformation which is the identity on
--   y coordinates and sends @(0,1)@ to @(d,1)@.
shearingX :: Num n => n -> T2 n
shearingX d =
  fromLinear
    (V2 (V2 1   d)  (V2 0 1))
    (V2 (V2 1 (-d)) (V2 0 1))
{-# INLINE shearingX #-}

-- | @shearX d@ performs a shear in the x-direction which sends
--   @(0,1)@ to @(d,1)@.
shearX :: (InSpace V2 n t, Transformable t) => n -> t -> t
shearX = transform . shearingX
{-# INLINE shearX #-}

-- | @shearingY d@ is the linear transformation which is the identity on
--   x coordinates and sends @(1,0)@ to @(1,d)@.
shearingY :: Num n => n -> T2 n
shearingY d =
  fromLinear
    (V2 (V2 1 0) (V2 d    1))
    (V2 (V2 1 0) (V2 (-d) 1))
{-# INLINE shearingY #-}

-- | @shearY d@ performs a shear in the y-direction which sends
--   @(1,0)@ to @(1,d)@.
shearY :: (InSpace V2 n t, Transformable t) => n -> t -> t
shearY = transform . shearingY
{-# INLINE shearY #-}
