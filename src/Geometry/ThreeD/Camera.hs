{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Camera
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Types to specify viewpoint for 3D rendering.
--
-----------------------------------------------------------------------------

module Geometry.ThreeD.Camera
  ( Camera  -- do not export constructor
  , cameraLocation
  , cameraAngle
  , cameraView
  , mm50Camera

  -- | Perspective lens
  , PerspectiveLens(..)
  , mm50
  , mm50Wide
  , mm50Narrow
  , fovx

  -- | Orthographic lens
  , OrthoLens(..)
  , orthoBounds
  -- , horizontalFieldOfView, verticalFieldOfView
  -- , orthoWidth, orthoHeight
  -- , camLoc, camForward, camUp, camRight, camLens
  -- , facing_ZCamera, mm50Camera
  -- , mm50, mm50Wide, mm50Narrow
  -- , aspect, camAspect
  )
  where

import           Control.Lens           ((^.), makeLenses)
-- import           Data.Monoid
import           Data.Typeable

import           Geometry.Angle
-- import           Geometry.Direction
-- import           Geometry.Transform
import           Geometry.Points
import           Geometry.Space
import           Geometry.ThreeD.Vector
import           Geometry.ThreeD.Transform
import Numeric.Interval

import           Linear.V3
import           Linear.Vector
import           Linear.Projection
import Linear.Matrix (M44)

-- Parameterize Camera on the lens type, so that Backends can express which
-- lenses they handle.
data Camera l n = Camera
  { cameraLocation :: !(Point V3 n)
  , cameraAngle    :: !(Euler n)
  , up             :: !(V3 n)
  , cameraLens     :: !(l n)
  } deriving Typeable

type instance V (Camera l n) = V3
type instance N (Camera l n) = n

class Typeable l => CameraLens l where
  -- | The natural aspect ratio of the projection.
  aspect :: Floating n => l n -> n

  -- | The projection of a lens as a homogeneous transformation matrix.
  lensProjection :: Floating n => l n -> M44 n

  -- | The inverse projection of a lens as a homogeneous transformation matrix.
  inverseLensProjection :: Floating n => l n -> M44 n

-- | The homogeneous view matrix for a camera, /not/ including the lens
--   projection.
cameraView :: Floating n => Camera l n -> M44 n
cameraView cam = undefined

instance CameraLens l => CameraLens (Camera l) where
  aspect = aspect . cameraLens
  lensProjection = lensProjection . cameraLens

-- Orthographic --------------------------------------------------------

-- | A perspective projection
data PerspectiveLens n = PerspectiveLens
  { _fovx  :: !(Angle n) -- ^ Horizontal field of view
  , _fovy  :: !(Angle n) -- ^ Vertical field of view
  , _nearz :: !n       -- ^ near clipping plane
  , _farz  :: !n       -- ^ far clipping plane
  }
  deriving Typeable

makeLenses ''PerspectiveLens

type instance V (PerspectiveLens n) = V3
type instance N (PerspectiveLens n) = n

instance CameraLens PerspectiveLens where
  aspect (PerspectiveLens h v _ _) = angleRatio h v
  lensProjection l = perspective (l^.fovy.rad) (aspect l) (l^.nearz) (l^.farz)
  inverseLensProjection l = inversePerspective (l^.fovy.rad) (aspect l) (l^.nearz) (l^.farz)

mm50, mm50Wide, mm50Narrow :: Floating n => PerspectiveLens n

-- | mm50 has the field of view of a 50mm lens on standard 35mm film,
-- hence an aspect ratio of 3:2.
mm50 = PerspectiveLens (40.5 @@ deg) (27 @@ deg) 0.1 1000

-- | mm50blWide has the same vertical field of view as mm50, but an
-- aspect ratio of 1.6, suitable for wide screen computer monitors.
mm50Wide = PerspectiveLens (43.2 @@ deg) (27 @@ deg) 0.1 1000

-- | mm50Narrow has the same vertical field of view as mm50, but an
-- aspect ratio of 4:3, for VGA and similar computer resolutions.
mm50Narrow = PerspectiveLens (36 @@ deg) (27 @@ deg) 0.1 1000

-- Orthographic --------------------------------------------------------

-- | An orthographic projection
data OrthoLens n = OrthoLens
  { _orthoWidth  :: n -- ^ Width
  , _orthoHeight :: n -- ^ Height
  , _orthoBounds :: Interval (V3 n)
  -- , _orthoRight :: Interval n
  -- , _orthoz :: Interval n
  }
  deriving Typeable

makeLenses ''OrthoLens

-- orthoRight, orthoLeft, orthoTom, orthoBottom, orthoNearZ, ortheFarX

type instance V (OrthoLens n) = V3
type instance N (OrthoLens n) = n

instance CameraLens OrthoLens where
  aspect o = o^.orthoHeight / o^.orthoWidth

-- instance Num n => Transformable (Camera l n) where
--   transform t (Camera p f u l) =
--       Camera (transform t p)
--              (transform t f)
--              (transform t u)
--              l

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
facing_ZCamera l = Camera origin (Euler zero zero zero) unitY l
{-# ANN facing_ZCamera ("HLint: ignore Use camelCase" :: String) #-}

-- | The direction the camera is facing. If the direction is directly up
--   or down, the camera's 'yaw' is preserved.
-- cameraForward :: Lens' (Camera l n) (Direction V3 n)
-- cameraForward = direction . forward

-- | The up direction used for a camera.
-- cameraUp :: Lens' (Camera l n) (Direction V3 n)
-- cameraUp = direction . up

-- | The direction to the right of the camera in the plane orthonal to
--   the up direction. Note: changing the right direction changes
--   the 'cameraUp' direction so that 'cameraRight' still lies in this
--   plane.
-- cameraRight :: Fractional n => Lens' (Camera l n) (Direction V3 n)
-- cameraRight c = direction right where
--   right = cross (forward c) (up c)

-- cameraLens :: Lens' (Camera l n) (l n)
-- cameraLens = lens

