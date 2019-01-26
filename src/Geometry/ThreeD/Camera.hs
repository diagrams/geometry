{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.ThreeD.Camera
-- Copyright   :  (c) 2013-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Types to specify viewpoint for 3D rendering.
--
-----------------------------------------------------------------------------

module Geometry.ThreeD.Camera
  (
  -- * Cameras
    Camera  -- do not export constructor
  , CameraLens (..)
  , cameraLocation
  , cameraAngle
  , cameraView
  , cameraLoc
  , mm50Camera

  -- * Perspective lens
  , PerspectiveLens(..)
  , mm50
  , mm50Wide
  , mm50Narrow
  , fovx

  -- * Orthographic lens
  , OrthoLens(..)
  , orthoBounds
  -- , horizontalFieldOfView, verticalFieldOfView
  -- , orthoWidth, orthoHeight
  -- , camLoc, camForward, camUp, camRight, camLens
  -- , facing_ZCamera, mm50Camera
  -- , mm50, mm50Wide, mm50Narrow
  -- , aspect, camAspect
  , camForwardRight
  , camForward
  , camUp
  , cameraLens
  )
  where

import           Control.Lens
import           Data.Typeable

import           Geometry.Angle
import           Geometry.Points
import           Geometry.Space
import           Geometry.ThreeD.Transform
import           Geometry.ThreeD.Types

import           Linear.Matrix             (M44, mkTransformationMat, transpose,
                                            (!*))
import           Linear.Projection
import           Linear.Vector

-- | A @Camera@ specifies a 3D viewpoint for rendering.  It is
--   parameterized on the lens type, so backends can express which
--   lenses they handle.
--
--   Note that the constructor is intentionally not exported; to
--   construct a @Camera@, XXX?
data Camera l n = Camera
  { cameraLocation :: !(P3 n)
  , cameraAngle    :: !(Euler n)
  , _cameraUp      :: !(V3 n)
  , camLens        :: !(l n)
  } deriving Typeable

type instance V (Camera l n) = V3
type instance N (Camera l n) = n

-- instance Num n => Transformable (Camera l n) where
--   transform t (Camera p f u l) =
--       Camera (transform t p)
--              (transform t f)
--              (transform t u)
--              l

class Typeable l => CameraLens l where
  -- | The natural aspect ratio of the projection.
  aspect :: Floating n => l n -> n

  -- | The projection of a lens as a homogeneous transformation matrix.
  lensProjection :: Floating n => l n -> M44 n

  -- | The inverse projection of a lens as a homogeneous transformation
  --   matrix.
  inverseLensProjection :: Floating n => l n -> M44 n

instance Rotational (Camera l) where
  euler f cam = f (cameraAngle cam) <&> \e -> cam {cameraAngle = e}

-- | The homogeneous view matrix for a camera, /not/ including the lens
--   projection.
cameraView :: RealFloat n => Camera l n -> M44 n
cameraView cam = mkTransformationMat m v
  where
  -- To get the view matrix we want the inverse of translating and then
  -- rotating the camera. The inverse of a rotation matrix is its
  -- transpose and the camera location is negated.
  m = transpose (rotationMatrix cam)
  v = m !* (-cam^.cameraLoc._Point)

cameraLoc :: Lens' (Camera l n) (P3 n)
cameraLoc f cam = f (cameraLocation cam) <&> \p -> cam {cameraLocation = p}

instance CameraLens l => CameraLens (Camera l) where
  aspect                = aspect                . camLens
  lensProjection        = lensProjection        . camLens
  inverseLensProjection = inverseLensProjection . camLens

-- Perspective ---------------------------------------------------------

-- | A perspective projection
data PerspectiveLens n = PerspectiveLens
  { _fovx  :: !(Angle n) -- ^ Horizontal field of view
  , _fovy  :: !(Angle n) -- ^ Vertical field of view
  , _nearz :: !n         -- ^ near clipping plane
  , _farz  :: !n         -- ^ far clipping plane
  }
  deriving Typeable

makeLenses ''PerspectiveLens

type instance V (PerspectiveLens n) = V3
type instance N (PerspectiveLens n) = n

instance CameraLens PerspectiveLens where
  aspect (PerspectiveLens h v _ _) = angleRatio h v
  lensProjection l = perspective (l^.fovy.rad) (aspect l) (l^.nearz) (l^.farz)
  inverseLensProjection l = inversePerspective (l^.fovy.rad) (aspect l) (l^.nearz) (l^.farz)

-- | mm50 has the field of view of a 50mm lens on standard 35mm film,
-- hence an aspect ratio of 3:2.
mm50 :: Floating n => PerspectiveLens n
mm50 = PerspectiveLens (40.5 @@ deg) (27 @@ deg) 0.1 1000

-- | mm50blWide has the same vertical field of view as mm50, but an
-- aspect ratio of 1.6, suitable for wide screen computer monitors.
mm50Wide :: Floating n => PerspectiveLens n
mm50Wide = PerspectiveLens (43.2 @@ deg) (27 @@ deg) 0.1 1000

-- | mm50Narrow has the same vertical field of view as mm50, but an
-- aspect ratio of 4:3, for VGA and similar computer resolutions.
mm50Narrow :: Floating n => PerspectiveLens n
mm50Narrow = PerspectiveLens (36 @@ deg) (27 @@ deg) 0.1 1000

-- Orthographic --------------------------------------------------------

-- | An orthographic projection
data OrthoLens n = OrthoLens
  { _orthoWidth  :: n -- ^ Width
  , _orthoHeight :: n -- ^ Height
  , _orthoBounds :: V3 (n,n)
  }
  deriving Typeable

makeLenses ''OrthoLens

-- orthoRight, orthoLeft, orthoTom, orthoBottom, orthoNearZ, ortheFarX

type instance V (OrthoLens n) = V3
type instance N (OrthoLens n) = n

instance CameraLens OrthoLens where
  aspect o = o^.orthoHeight / o^.orthoWidth
  lensProjection orthoLens = ortho l r b t n f where
    V3 (l,r) (b,t) (n,f) = orthoLens^.orthoBounds
  inverseLensProjection orthoLens = inverseOrtho l r b t n f where
    V3 (l,r) (b,t) (n,f) = orthoLens^.orthoBounds

-- | A camera at the origin facing along the negative Z axis, with its
-- up-axis coincident with the positive Y axis. The field of view is
-- chosen to match a 50mm camera on 35mm film. Note that Cameras take
-- up no space in the Diagram.
mm50Camera :: Floating n => Camera PerspectiveLens n
mm50Camera = facing_ZCamera mm50

-- | 'facing_ZCamera l' is a camera at the origin facing along the
-- negative Z axis, with its up-axis coincident with the positive Y
-- axis, with the projection defined by l.
facing_ZCamera :: Num n => l n -> Camera l n
facing_ZCamera = Camera origin (Euler zero zero zero) (V3 0 1 0)
{-# ANN facing_ZCamera ("HLint: ignore Use camelCase" :: String) #-}

-- | The unit forward and right directions.
camForwardRight :: RealFloat n => Camera l n -> (V3 n, V3 n)
camForwardRight cam = (fw, V3 cy 0 (-sy))
  where
    fw = V3 (-sy*cp) sp (-cy*cp) -- - ^/ sqrt (1 + sp*sp)
    y  = cam^.yaw
    p  = cam^.pitch
    sy = sinA y
    cy = cosA y
    sp = sinA p
    cp = cosA p
{-# INLINE camForwardRight #-}

camUp :: RealFloat n => Lens' (Camera l n) (V3 n)
camUp f (Camera loc angle up l) = f up <&> \up' -> Camera loc angle up' l

camForward :: RealFloat n => Lens' (Camera l n) (V3 n)
camForward f cam = f (fst $ camForwardRight cam) <&> \v ->
  cam & pitch .~ atan2A (v^._x) (-v^._z)
      & yaw   .~ acosA (v^._z)

-- | The lens used for the camera.
cameraLens :: Lens (Camera l n) (Camera l' n) (l n) (l' n)
cameraLens f (Camera loc angle up l) = f l <&> Camera loc angle up

