{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.ThreeD.Transform
-- Copyright   :  (c) 2013-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Transformations specific to three dimensions, with a few generic
-- transformations (uniform scaling, translation) also re-exported for
-- convenience.
--
-----------------------------------------------------------------------------

module Geometry.ThreeD.Transform
  ( T3

    -- * Rotation
  , aboutX, aboutY, aboutZ
  , rotationAbout, rotateAbout
  , pointAt, pointAt'

    -- ** Euler angles
  , Euler (..), yaw, pitch, roll

    -- ** Rotational class
  , Rotational (..)
  , rotateWith

  -- * Scaling
  , scalingX, scalingY, scalingZ
  , scaleX, scaleY, scaleZ
  , scaling, scale

  -- * Translation
  , translationX, translateX
  , translationY, translateY
  , translationZ, translateZ
  , translation, translate

    -- * Reflection
  , reflectionX, reflectX
  , reflectionY, reflectY
  , reflectionZ, reflectZ
  , reflectionAcross, reflectAcross

  ) where

import           Geometry.Angle
import           Geometry.Direction
import           Geometry.Points
import           Geometry.Space
import           Geometry.ThreeD.Types
import           Geometry.Transform
import           Geometry.TwoD.Transform
import qualified Data.Semigroup as Sem

import           Control.Lens            hiding (transform)
import           Data.Functor.Rep

import           Linear                  (cross, dot, signorm)
import           Linear.Matrix           hiding (translation)
import           Linear.Quaternion
import           Linear.Vector

-- | Create a transformation which rotates by the given angle about
--   a line parallel the Z axis passing through the local origin.
--   A positive angle brings positive x-values towards the positive-y axis.
--
--   The angle can be expressed using any type which is an
--   instance of 'Angle'.  For example, @aboutZ (1\/4 \@\@
--   'turn')@, @aboutZ (tau\/4 \@\@ 'rad')@, and @aboutZ (90 \@\@
--   'deg')@ all represent the same transformation, namely, a
--   counterclockwise rotation by a right angle.  For more general rotations,
--   see 'rotationAbout'.
--
--   Note that writing @aboutZ (1\/4)@, with no type annotation, will
--   yield an error since GHC cannot figure out which sort of angle
--   you want to use.
aboutZ :: Floating n => Angle n -> T3 n
aboutZ a = fromOrthogonal $
  V3 (V3 c    s 0)
     (V3 (-s) c 0)
     (V3 0    0 1)
  where s = sinA a; c = cosA a

-- | Like 'aboutZ', but rotates about the X axis, bringing positive y-values
-- towards the positive z-axis.
aboutX :: Floating n => Angle n -> T3 n
aboutX a = fromOrthogonal $
  V3 (V3 1   0  0)
     (V3 0   c  s)
     (V3 0 (-s) c)
  where s = sinA a; c = cosA a

-- | Like 'aboutZ', but rotates about the Y axis, bringing postive
-- x-values towards the negative z-axis.
aboutY :: Floating n => Angle n -> T3 n
aboutY a = fromOrthogonal $
  V3 (V3 c 0 (-s))
     (V3 0 1   0 )
     (V3 s 0   c )
  where s = sinA a; c = cosA a

-- | @rotationAbout p d a@ is a rotation about a line parallel to @d@
--   passing through @p@.
rotationAbout
  :: Floating n
  => P3 n               -- ^ origin of rotation
  -> Direction V3 n     -- ^ direction of rotation axis
  -> Angle n            -- ^ angle of rotation
  -> T3 n
rotationAbout (P p) d a = conjugate (translation p) (axisRotation d a)

axisRotation :: Floating n => Direction V3 n -> Angle n -> T3 n
axisRotation d a = fromOrthogonal $
  V3 (V3 (t*x*x + c)   (t*x*y - z*s) (t*x*z + y*s))
     (V3 (t*x*y + z*s) (t*y*y + c)   (t*y*z - x*s))
     (V3 (t*x*z - y*s) (t*y*z + x*s) (t*z*z + c))
  where
  c = cosA a
  s = sinA a
  t = 1 - c
  V3 x y z = fromDirection d

-- | @rotateAbout p d a@ rotates about a line parallel to @d@ passing
--   through @p@.
rotateAbout
  :: (InSpace V3 n t, Floating n, Transformable t)
  => P3 n            -- ^ origin of rotation
  -> Direction V3 n  -- ^ direction of rotation axis
  -> Angle n         -- ^ angle of rotation
  -> t -> t
rotateAbout p d theta = transform (rotationAbout p d theta)

-- | @pointAt about initial final@ produces a rotation which brings
-- the direction @initial@ to point in the direction @final@ by first
-- panning around @about@, then tilting about the axis perpendicular
-- to @about@ and @final@.  In particular, if this can be accomplished
-- without tilting, it will be, otherwise if only tilting is
-- necessary, no panning will occur.  The tilt will always be between
-- ± 1/4 turn.
pointAt :: Floating n
        => Direction V3 n -> Direction V3 n -> Direction V3 n
        -> Transformation V3 n
pointAt a i f = pointAt' (fromDirection a) (fromDirection i) (fromDirection f)

-- | pointAt' has the same behavior as 'pointAt', but takes vectors
-- instead of directions.
pointAt' :: Floating n => V3 n -> V3 n -> V3 n -> Transformation V3 n
pointAt' about initial final = pointAtUnit (signorm about) (signorm initial) (signorm final)

-- | pointAtUnit has the same behavior as @pointAt@, but takes unit vectors.
pointAtUnit :: Floating n => V3 n -> V3 n -> V3 n -> Transformation V3 n
pointAtUnit about initial final = tilt Sem.<> pan where
  -- rotating u by (signedAngle rel u v) about rel gives a vector in the direction of v
  signedAngle rel u v = signum (cross u v `dot` rel) *^ angleBetween u v
  inPanPlaneF = final ^-^ project about final
  inPanPlaneI = initial ^-^ project about initial
  panAngle    = signedAngle about inPanPlaneI inPanPlaneF
  pan         = rotationAbout origin (direction about) panAngle
  tiltAngle   = signedAngle tiltAxis (transform pan initial) final
  tiltAxis    = cross final about
  tilt        = rotationAbout origin (direction tiltAxis) tiltAngle

-- Scaling -------------------------------------------------

-- | Construct a transformation which scales by the given factor in
--   the z direction.
scalingZ :: (HasBasis v, R3 v, Fractional n) => n -> Transformation v n
scalingZ c =
  fromLinear
    (eye & _z . _z .~ c)
    (eye & _z . _z //~ c)
{-# INLINE scalingZ #-}

-- | Scale an object by the given factor in the z direction.  To scale
-- uniformly, use 'scale'.
scaleZ :: (InSpace v n t, HasBasis v, R3 v, Fractional n, Transformable t) => n -> t -> t
scaleZ = transform . scalingZ
{-# INLINE scaleZ #-}

-- Translation ----------------------------------------

-- | Construct a transformation which translates by the given distance
--   in the z direction.
translationZ :: (HasBasis v, R3 v, Num n) => n -> Transformation v n
translationZ z = translation (zero & _z .~ z)
{-# INLINE translationZ #-}

-- | Translate an object by the given distance in the y
--   direction.
translateZ :: (InSpace v n t, HasBasis v, R3 v, Transformable t) => n -> t -> t
translateZ = transform . translationZ
{-# INLINE translateZ #-}

-- Reflection ----------------------------------------------

-- | Construct a transformation which flips an object across the line \(z=0\),
--   i.e. sends the point \((x,y,z)\) to \((x,y,-z)\).
reflectionZ :: (HasBasis v, R3 v, Num n) => Transformation v n
reflectionZ = fromInvoluted $ eye & _z . _z .~ (-1)
{-# INLINE reflectionZ #-}

-- | Flip an object across the line \(z=0\), i.e. send the point \((x,y,z)\) to
--   \((x,y,-z)\).
reflectZ :: (InSpace v n t, HasBasis v, R3 v, Transformable t) => t -> t
reflectZ = transform reflectionZ
{-# INLINE reflectZ #-}

-- | @reflectionAcross p v@ is a reflection across the plane through
--   the point @p@ and normal to vector @v@. This also works as a 2D
--   transform where @v@ is the normal to the line passing through point
--   @p@.
reflectionAcross :: (HasLinearMap v, Fractional n)
  => Point v n -> v n -> Transformation v n
reflectionAcross p v =
  conjugate (translation (origin .-. p)) reflect
    where
      reflect = fromLinear (f v) (f (negated v))
      f u     = eye & fmapRep (\w -> w ^-^ 2 *^ project u w)

-- | @reflectAcross p v@ reflects an object across the plane though
--   the point @p@ and the vector @v@. This also works as a 2D transform
--   where @v@ is the normal to the line passing through point @p@.
reflectAcross :: (InSpace v n t, HasLinearMap v, Transformable t, Fractional n)
  => Point v n -> v n -> t -> t
reflectAcross p v = transform (reflectionAcross p v)

-- | Things representing 3D rotations.
class Rotational t where
  {-# MINIMAL quaternion | euler #-}
  -- | Lens onto the rotational transform as a quaternion.
  quaternion :: RealFloat n => Lens' (t n) (Quaternion n)
  quaternion = euler . iso e2q q2e
  {-# INLINE quaternion #-}

  -- | Lens onto the rotational transform as an Euler angle.
  euler :: RealFloat n => Lens' (t n) (Euler n)
  euler = quaternion . iso q2e e2q
  {-# INLINE euler #-}

  -- | Lens onto the axis angle representation of a rotation.
  axisAngle :: RealFloat n => Lens' (t n) (AxisAngle n)
  axisAngle = quaternion . iso q2aa aa2q

  -- | The rotational component as a 3x3 matrix.
  rotationMatrix :: RealFloat n => t n -> M33 n
  rotationMatrix = fromQuaternion . view quaternion
  {-# INLINE rotationMatrix #-}

  -- | The rotational component as a 'Transformation'.
  rotationTransform :: RealFloat n => t n -> T3 n
  rotationTransform = fromOrthogonal . rotationMatrix
  {-# INLINE rotationTransform #-}

-- | Unit 'Quaternion's only.
instance Rotational Quaternion where
  quaternion = iso id id
  {-# INLINE quaternion #-}

-- | Rotate something in 3D space.
rotateWith :: (InSpace V3 n a, Rotational t, Transformable a, RealFloat n) => t n -> a -> a
rotateWith = transform . rotationTransform
{-# INLINE rotateWith #-}

------------------------------------------------------------------------
-- Euler Angles
------------------------------------------------------------------------

-- | Describe a rotational transform as a 'yaw', 'pitch' and 'roll'.
--   Currently uses Tait–Bryan YXZ convension. That is, 'yaw' rotates
--   around the y-axis, then 'pitch' rotates around the x-axis, then
--   roll rotates around the z-axis (is this the right order?).
data Euler n = Euler !(Angle n) !(Angle n) !(Angle n)
  deriving (Show, Read, Functor)

q2e :: RealFloat n => Quaternion n -> Euler n
q2e (Quaternion qw (V3 qx qy qz)) = Euler y p r
  where
    t0 =     2*(qw*qy + qz*qx)
    t1 = 1 - 2*(qx*qx + qy*qy)
    t2 =     2*(qw*qx - qz*qy)
    t3 =     2*(qw*qz + qx*qy)
    t4 = 1 - 2*(qz*qz + qx*qx)
    -- account for floating point errors
    t2' | t2 >  1   =  1
        | t2 < -1   = -1
        | otherwise = t2
    --
    y = atan2A t0 t1
    p = asinA t2'
    r = atan2A t3 t4
{-# INLINE q2e #-}

e2q :: Floating n => Euler n -> Quaternion n
e2q (Euler y p r) = Quaternion qw (V3 qx qy qz)
  where
    qw = cr*cp*cy + sr*sp*sy
    qz = sr*cp*cy - cr*sp*sy
    qx = cr*sp*cy + sr*cp*sy
    qy = cr*cp*sy - sr*sp*cy
    --
    cy = cosA (0.5*^y)
    sy = sinA (0.5*^y)
    cp = cosA (0.5*^p)
    sp = sinA (0.5*^p)
    cr = cosA (0.5*^r)
    sr = sinA (0.5*^r)
{-# INLINE e2q #-}

instance Rotational Euler where
  euler = iso id id -- stupid redundant constraint checker
  {-# INLINE euler #-}
-- is it worth making a unit quaternion type wrapper?

yaw :: (Rotational t, RealFloat n) => Lens' (t n) (Angle n)
yaw = euler . (\f (Euler y p r) -> f y <&> \y' -> Euler y' p r)
{-# INLINE yaw #-}

pitch :: (Rotational t, RealFloat n) => Lens' (t n) (Angle n)
pitch = euler . (\f (Euler y p r) -> f p <&> \p' -> Euler y p' r)
{-# INLINE pitch #-}

roll :: (Rotational t, RealFloat n) => Lens' (t n) (Angle n)
roll = euler . (\f (Euler y p r) -> f r <&> \r' -> Euler y p r')
{-# INLINE roll #-}

------------------------------------------------------------------------
-- Axis angle
------------------------------------------------------------------------

-- | An axis angle, represented by a unit vector v and an angle around
--   that vector.
data AxisAngle n = AxisAngle !(V3 n) !(Angle n)
  deriving Show

q2aa :: RealFloat n => Quaternion n -> AxisAngle n
q2aa (Quaternion q0 v) = AxisAngle (v ^/ t) (2 *^ atan2A t q0)
  where t = sqrt (1 - q0*q0)
{-# INLINE q2aa #-}

aa2q :: Floating n => AxisAngle n -> Quaternion n
aa2q (AxisAngle axis theta) = Quaternion (cosA half) (sinA half *^ axis)
  where half = theta ^/ 2
{-# INLINE aa2q #-}

instance Rotational AxisAngle where
  quaternion = iso aa2q q2aa
  {-# INLINE quaternion #-}
  axisAngle = iso id id
  {-# INLINE axisAngle #-}

------------------------------------------------------------------------
-- Rotation matrix
------------------------------------------------------------------------

-- Is this worth doing?

-- | A matrix representing a rotation.
-- newtype RotationMatrix n = RM (M33 n)

-- q2rm :: RealFloat n => RotationMatrix n -> Quaternion n
-- rm2q :: RealFloat n => Quaternion n -> RotationMatrix n
-- instance Rotational RotationMatrix where
--   quaternion = iso rm2q q2rm

