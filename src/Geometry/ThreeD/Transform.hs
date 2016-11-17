{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Transform
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
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
  -- , pointAt, pointAt'
  , Euler (..)
  , Rotational (..)

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

import           Geometry.Transform

import           Geometry.Angle
import           Geometry.Direction
import           Geometry.Space
import           Geometry.TwoD.Transform
import           Geometry.ThreeD.Types

import           Control.Lens hiding (transform)
-- import           Data.Semigroup

import           Linear.Metric
import           Linear.Matrix hiding (translation)
import           Linear.V3 (cross)
import           Linear.Affine
import           Linear.Vector
import           Linear.Quaternion

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

-- | @rotationAbout p d a@ is a rotation about a line parallel to @d@
--   passing through @p@.
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
-- pointAt :: Floating n
--         => Direction V3 n -> Direction V3 n -> Direction V3 n
--         -> Transformation V3 n
-- pointAt a i f = pointAt' (fromDirection a) (fromDirection i) (fromDirection f)

-- | pointAt' has the same behavior as 'pointAt', but takes vectors
-- instead of directions.
-- pointAt' :: Floating n => V3 n -> V3 n -> V3 n -> Transformation V3 n
-- pointAt' about initial final = pointAtUnit (signorm about) (signorm initial) (signorm final)

-- | pointAtUnit has the same behavior as @pointAt@, but takes unit vectors.
-- pointAtUnit :: Floating n => V3 n -> V3 n -> V3 n -> Transformation V3 n
-- pointAtUnit about initial final = tilt <> pan where
--   -- rotating u by (signedAngle rel u v) about rel gives a vector in the direction of v
--   signedAngle rel u v = signum (cross u v `dot` rel) *^ angleBetween u v
--   inPanPlaneF = final ^-^ project about final
--   inPanPlaneI = initial ^-^ project about initial
--   panAngle    = signedAngle about inPanPlaneI inPanPlaneF
--   pan         = rotationAbout origin (direction about) panAngle
--   tiltAngle   = signedAngle tiltAxis (transform pan initial) final
--   tiltAxis    = cross final about
--   tilt        = rotationAbout origin (direction tiltAxis) tiltAngle

-- Scaling -------------------------------------------------

-- | Construct a transformation which scales by the given factor in
--   the z direction.
scalingZ :: (HasBasis v, R3 v, Fractional n) => n -> Transformation v n
scalingZ c =
  fromLinear
    (eye & _z . _z .~ c)
    (eye & _z . _z //~ c)
{-# INLINE scalingZ #-}

-- | Scale a diagram by the given factor in the z direction.  To scale
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

-- | Translate a diagram by the given distance in the y
--   direction.
translateZ :: (InSpace v n t, HasBasis v, R3 v, Transformable t) => n -> t -> t
translateZ = transform . translationZ
{-# INLINE translateZ #-}

-- Reflection ----------------------------------------------

-- | Construct a transformation which flips a diagram across z=0,
-- i.e. sends the point (x,y,z) to (x,y,-z).
reflectionZ :: (HasBasis v, R3 v, Num n) => Transformation v n
reflectionZ = fromInvoluted $ eye & _z . _z .~ (-1)
{-# INLINE reflectionZ #-}

-- | Flip a diagram across z=0, i.e. send the point (x,y,z) to
-- (x,y,-z).
reflectZ :: (InSpace v n t, HasBasis v, R3 v, Transformable t) => t -> t
reflectZ = transform reflectionZ
{-# INLINE reflectZ #-}

-- | @reflectionAcross p v@ is a reflection across the plane through
--   the point @p@ and normal to vector @v@. This also works as a 2D
--   transform where @v@ is the normal to the line passing through point
--   @p@.
reflectionAcross :: (Metric v, Fractional n)
  => Point v n -> v n -> Transformation v n
reflectionAcross = undefined -- p v
  -- conjugate (translation (origin .-. p)) reflect
  --   where
  --     reflect = fromLinear t (linv t)
  --     t       = f v <-> f (negated v)
  --     f u w   = w ^-^ 2 *^ project u w

-- | @reflectAcross p v@ reflects a diagram across the plane though
--   the point @p@ and the vector @v@. This also works as a 2D transform
--   where @v@ is the normal to the line passing through point @p@.
reflectAcross :: (InSpace v n t, Metric v, Fractional n, Transformable t)
  => Point v n -> v n -> t -> t
reflectAcross p v = transform (reflectionAcross p v)

-- | Class of things which describe a rotational transformation in 3D
--   space.
class Rotational t where
  {-# MINIMAL quaternion | euler #-}
  -- | Lens onto the rotational transform as a quaternion.
  quaternion :: RealFloat n => Lens' (t n) (Quaternion n)
  quaternion = euler . iso euler_to_quat quat_to_euler
  {-# INLINE quaternion #-}

  -- | Lens onto the rotational transform as an euler angle.
  euler :: RealFloat n => Lens' (t n) (Euler n)
  euler = quaternion . iso quat_to_euler euler_to_quat
  {-# INLINE euler #-}

  -- | The rotational component as a 3x3 matrix.
  rotationMatrix :: RealFloat n => t n -> M33 n
  rotationMatrix = fromQuaternion . view quaternion
  {-# INLINE rotationMatrix #-}

  -- | The rotational component as a 'Transformation'.
  rotationTransform :: RealFloat n => t n -> T3 n
  rotationTransform t = T (fromQuaternion q) (fromQuaternion (recip q)) zero
    where q = view quaternion t

-- -- Euler angles --------------------------------------------------------

-- -- | Describe a rotational transform as a 'yaw', 'pitch' and 'roll'.
-- --   Currently only uses the YXZ convension.
-- --
-- -- XXX Need to state which convension I'm using!
data Euler n = Euler !(Angle n) !(Angle n) !(Angle n)
  deriving Show

quat_to_euler :: RealFloat n => Quaternion n -> Euler n
quat_to_euler (Quaternion q0 (V3 q1 q2 q3)) = Euler y p r
  where
    y = asinA (clamp (-1) 1 (2*(q0*q2 + q3*q1)))
    p = atan2A (2*(q0*q1 - q2*q3)) (1 - 2*(q1*q1+q2*q2))
    r = atan2A (2*(q1*q2 - q0*q3)) (1 - 2*(q2*q2+q3*q3))

clamp :: Ord n => n -> n -> n -> n
clamp a b x
  | x < a     = a
  | x > b     = b
  | otherwise = x

-- takes a unit vector
axAngle :: Floating a => V3 a -> Angle a -> Quaternion a
axAngle axis theta = Quaternion (cosA half) (sinA half *^ axis)
  where half = theta ^/ 2
{-# INLINE axAngle #-}

infixl 7 `qm`
qm :: Num n => Quaternion n -> Quaternion n -> Quaternion n
Quaternion s1 v1 `qm` Quaternion s2 v2 =
    Quaternion (s1*s2 - (v1 `dot` v2)) ((v1 `cross` v2) + s1*^v2 + s2*^v1)

-- eulerQuat :: RealFloat n => Iso' (Euler n) (Quaternion n)
-- eulerQuat = iso euler_to_quat quat_to_euler

euler_to_quat :: Floating n => Euler n -> Quaternion n
euler_to_quat (Euler y p r) =
  axAngle (V3 0 0 1) r `qm`
  axAngle (V3 1 0 0) p `qm`
  axAngle (V3 0 1 0) y

-- euler_to_quat_inverse :: Floating n => Euler n -> Quaternion n
-- euler_to_quat_inverse (Euler y p r) =
--   axAngle (V3 0 0 1) (negated y) `qm`
--   axAngle (V3 1 0 0) (negated p) `qm`
--   axAngle (V3 0 0 1) (negated r)

-- -- | 'M33' can't be made an instance of Rotational because of type
-- --   parameters. (maybe I should change it to use N?)
-- -- m33Quat :: RealFloat n => Lens' (M33 n) (Quaternion n)
-- -- m33Euler :: RealFloat n => Lens' (M33 n) (Euler n)

-- -- instance Rotational T3
-- instance Rotational Euler where
--   euler =  id
--   {-# INLINE euler #-}

-- instance Rotational Quaternion where
--   quaternion = id
--   {-# INLINE quaternion #-}

-- -- instance Rotational Camera

-- -- yaw :: (Rotational t, RealFloat n) => Lens' (t n) (Angle n)
-- -- yaw = euler . (\f (Euler y p r) -> f y <&> y' -> Euler y' p r)
-- -- {-# INLINE yaw #-}

-- -- pitch :: (Rotational t, RealFloat n) => Lens' (t n) (Angle n)
-- -- pitch = euler . (\f (Euler y p r) -> f p <&> p' -> Euler y p' r)
-- -- {-# INLINE pitch #-}

-- -- roll :: (Rotational t, RealFloat n) => Lens' (t n) (Angle n)
-- -- roll = euler . (\f (Euler y p r) -> f r <&> r' -> Euler y p r')
-- -- {-# INLINE roll #-}

