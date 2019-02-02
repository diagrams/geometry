{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.ThreeD.Shapes
-- Copyright   :  (c) 2016-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Various three-dimensional shapes.
--
-----------------------------------------------------------------------------

module Geometry.ThreeD.Shapes
  (
    -- * Basic 3D shapes
    Sphere (..)
  , EllipsoidLike (..)

  , Cube (..)
  , CuboidLike (..)
  , cuboid

  , Frustum (..)
  , FrustumLike (..)
  , cone
  , cylinder

    -- * Constructive solid geometry
  , CSG(..)
  , union
  , intersection
  , difference
  ) where

import           Control.Lens                       (review, (^.), _1)
import           Data.Semigroup
import qualified Data.Sequence                      as Seq
import           Data.Typeable

import           Geometry.Angle
import           Geometry.BoundingBox
import           Geometry.Direction
import           Geometry.Envelope
import           Geometry.Points
import           Geometry.Query
import           Geometry.Space
import           Geometry.ThreeD.Types
import           Geometry.ThreeD.Vector
import           Geometry.Trace
import           Geometry.Transform

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector
import           Numeric.Interval.NonEmpty.Internal hiding (intersection)

-- Ellipsoid -----------------------------------------------------------

data Sphere n = Sphere
  deriving Typeable

type instance V (Sphere n) = V3
type instance N (Sphere n) = n

instance (Num n, Ord n) => HasQuery (Sphere n) Any where
  getQuery Sphere = Query $ \(P v) -> Any $ quadrance v <= 1

instance OrderedField n => Enveloped (Sphere n) where
  getEnvelope Sphere = Envelope (const $ I (-1) 1)

instance OrderedField n => Traced (Sphere n) where
  getTrace Sphere = mkTrace $ \(P p) v -> let
    a  =    v `dot` v
    b  = 2 * (p `dot` v)
    c  =    (p `dot` p) - 1
    in
     -- mkSortedList $ quadForm a b c
     Seq.fromList $ error "need quadForm" a b c

class EllipsoidLike t where
  -- | A sphere of radius 1 with its center at the origin.
  sphere :: t

-- Cuboid --------------------------------------------------------------

data Cube n = Cube
  deriving Typeable

type instance V (Cube n) = V3
type instance N (Cube n) = n

-- instance Fractional n => Transformable (Cube n) where
--   transform t1 (Cube t2) = Cube (t1 <> t2)

instance OrderedField n => Enveloped (Cube n) where
  getEnvelope Cube = getEnvelope (fromCorners origin (mkP3 1 1 1))
    -- mkEnvelope $ \v ->
    -- maximum (map (v `dot`) corners) where
    --   corners = mkR3 <$> [0,1] <*> [0,1] <*> [0,1]

instance (Fractional n, Ord n) => Traced (Cube n) where
  getTrace Cube = mkTrace $ \p v -> let
    (x0, y0, z0) = unp3 p
    (vx, vy, vz) = unr3 v
    intersections f d = case d of
      0 -> []
      _ -> [-f/d, (1-f)/d]
    ts = concat $ zipWith intersections [x0,y0,z0] [vx,vy,vz]
    atT t = p .+^ (t*^v)
    range u = and [x >= 0, x <= 1, y >= 0, y <= 1, z >= 0, z <= 1] where
      (x, y, z) = unp3 u
    in
     -- ts gives all intersections with the planes forming the cube
     -- filter keeps only those actually on the cube surface
     Seq.fromList . filter (range . atT) $ ts where

instance (Num n, Ord n) => HasQuery (Cube n) Any where
  getQuery Cube = Query $ Any . range where
    range u = and [x >= 0, x <= 1, y >= 0, y <= 1, z >= 0, z <= 1] where
      (x, y, z) = unp3 u

class CuboidLike t where
  cube :: t

-- | A cubeoid with corners @a@ and @b@.
cuboid :: (InSpace V3 n t, CuboidLike t, Transformable t, Fractional n) => P3 n -> P3 n -> t
cuboid a b = transform t cube
  where
    t = scalingV (a .-. b) <> translation (a^._Point)

-- Frustum -------------------------------------------------------------

-- | A conical frustum.
data Frustum n = Frustum n n
  deriving Typeable

type instance V (Frustum n) = V3
type instance N (Frustum n) = n

-- instance Fractional n => Transformable (Frustum n) where
--   transform t1 (Frustum r0 r1 t2) = Frustum r0 r1 (t1 <> t2)

envelope :: OrderedField n => [n] -> Interval n
envelope = foldr (\x (I a b) -> I (min x a) (max x b)) (I (1/0) (-1/0))

instance (OrderedField n, RealFloat n) => Enveloped (Frustum n) where
  -- The plane containing v and the z axis intersects the frustum in a trapezoid
  -- Test the four corners of this trapezoid; one must determine the Envelope
  getEnvelope (Frustum r0 r1) = Envelope $ \(Dir v) ->
    let θ       = v ^. _theta
        corners = [(r1,θ,1), (-r1,θ,1), (r0,θ,0), (-r0,θ,0)]
     in envelope . map (norm . project v . review r3CylindricalIso) $ corners

instance (RealFloat n, Ord n) => Traced (Frustum n) where
  -- The trace can intersect the sides of the cone or one of the end
  -- caps The sides are described by a quadric equation; substitute
  -- in the parametric form of the ray but disregard any
  -- intersections outside z = [0,1] Similarly, find intersections
  -- with the planes z=0, z=1, but disregard any r>r0, r>r1
  getTrace (Frustum r0 r1) = mkTrace $ \p v -> let
    (px, py, pz) = unp3 p
    (vx, vy, vz) = unr3 v
    ray t = p .+^ t *^ v
    dr = r1 - r0
    a = vx**2 + vy**2 - vz**2 * dr**2
    b = 2 * (px * vx + py * vy - (r0+pz*dr) * dr  * vz)
    c = px**2 + py**2 - (r0 + dr*pz)**2
    zbounds t = ray t ^. _z >= 0
         && ray t ^. _z <= 1
    ends = concatMap cap [0,1]
    cap z = [ t | ray t ^. lensP . r3CylindricalIso . _1 < r0 + z * dr ]
      where
      t = (z - pz) / vz
    in
     -- mkSortedList $ filter zbounds (quadForm a b c) ++ ends
     Seq.fromList $ filter zbounds (error "Traced Frustum not yet implimented" a b c) ++ ends

instance OrderedField n => HasQuery (Frustum n) Any where
  getQuery (Frustum r0 r1) =
    Query $ \p -> let
      z = p^._z
      r = r0 + (r1 - r0)*z
      v = p .-. origin
      a = norm $ projectXY v
      projectXY u = u ^-^ project unitZ u
      in
       Any $ z >= 0 && z <= 1 && a <= r

class Num (N t) => FrustumLike t where
  frustum :: N t -> N t -> t

instance Num n => FrustumLike (Frustum n) where
  -- | A frustum of a right circular cone.  It has height 1 oriented
  -- along the positive z axis, and radii r0 and r1 at Z=0 and Z=1.
  -- 'cone' and 'cylinder' are special cases.
  frustum = Frustum

-- | A cone with its base centered on the origin, with radius 1 at the
-- base, height 1, and it's apex on the positive Z axis.
-- cone :: Num n => Frustum n
-- cone = frustum 1 0
cone :: FrustumLike t => t
cone = frustum 1 0

-- | A circular cylinder of radius 1 with one end cap centered on the
-- origin, and extending to Z=1.
cylinder :: FrustumLike t => t
cylinder = frustum 1 1

-- CSG -----------------------------------------------------------------

-- The CSG type needs to form a tree to be useful.  This
-- implementation requires Backends to support all the included
-- primitives.  If that turns out to be a problem, we have several
-- options:
-- a) accept runtime errors for unsupported primitives
-- b) carry the set of primitives in a row type in the CSG type
-- c) implement CSG in Haskell, so Backends supporting triangle meshes
--    can fall back to those.
-- (c) is worth doing anyway; I'm ambivalent about the others.  -DMB

-- | A tree of Constructive Solid Geometry operations and the primitives that
-- can be used in them.
data CSG n
  = CsgEllipsoid (T3 n)
  | CsgBox !(T3 n)
  | CsgFrustum !n !n !(T3 n)
  | CsgUnion [CSG n]
  | CsgIntersection [CSG n]
  | CsgDifference (CSG n) (CSG n)
  deriving Typeable

type instance V (CSG n) = V3
type instance N (CSG n) = n

instance Fractional n => Transformable (CSG n) where
  transform t = \case
    CsgEllipsoid p      -> CsgEllipsoid $ transform t p
    CsgBox p            -> CsgBox $ transform t p
    CsgFrustum a b p    -> CsgFrustum a b $ transform t p
    CsgUnion ps         -> CsgUnion $ map (transform t) ps
    CsgIntersection ps  -> CsgIntersection $ map (transform t) ps
    CsgDifference p1 p2 -> CsgDifference (transform t p1) (transform t p2)

-- | The Envelope for an Intersection or Difference is simply the
-- Envelope of the Union.  This is wrong but easy to implement.
instance RealFloat n => Enveloped (CSG n) where
  getEnvelope = \case
    CsgEllipsoid t      -> transform t $ getEnvelope Sphere
    CsgBox t            -> transform t $ getEnvelope Cube
    CsgFrustum a b t    -> transform t $ getEnvelope (Frustum a b)
    CsgUnion ps         -> foldMap getEnvelope ps
    CsgIntersection ps  -> foldMap getEnvelope ps
    CsgDifference p1 p2 -> getEnvelope p1 <> getEnvelope p2
-- TODO after implementing some approximation scheme, calculate
-- correct (approximate) envelopes for intersections and difference.

instance (Floating n, Ord n) => HasQuery (CSG n) Any where
  getQuery = \case
    CsgEllipsoid t      -> transform t $ getQuery Sphere
    CsgBox t            -> transform t $ getQuery Cube
    CsgFrustum a b t    -> transform t $ getQuery (Frustum a b)
    CsgUnion ps         -> foldMap getQuery ps
    CsgIntersection ps  -> Any . getAll <$> foldMap (fmap (All . getAny) . getQuery) ps
    CsgDifference p1 p2 -> inOut <$> getQuery p1 <*> getQuery p2 where
      inOut (Any a) (Any b) = Any $ a && not b

instance (RealFloat n, Ord n) => Traced (CSG n) where
  getTrace = \case
    CsgEllipsoid t   -> transform t $ getTrace Sphere
    CsgBox t         -> transform t $ getTrace Cube
    CsgFrustum a b t -> transform t $ getTrace (Frustum a b)

    -- on surface of some p, and not inside any of the others
    CsgUnion []     -> mempty
    CsgUnion (s:ss) -> mkTrace t where
      t pt v = (Seq.filter $ without s) (appTrace (getTrace (CsgUnion ss)) pt v)
            <> (Seq.filter $ without (CsgUnion ss)) (appTrace (getTrace s) pt v) where
        newPt dist = pt .+^ v ^* dist
        without prim = not . inquire prim . newPt

    -- on surface of some p, and inside all the others
    CsgIntersection []     -> mempty
    CsgIntersection (s:ss) -> mkTrace t where
      t pt v = (Seq.filter $ within s) (appTrace (getTrace (CsgIntersection ss)) pt v)
            <> (Seq.filter $ within (CsgIntersection ss)) (appTrace (getTrace s) pt v) where
        newPt dist = pt .+^ v ^* dist
        within prim = inquire prim . newPt

    -- on surface of p1, outside p2, or on surface of p2, inside p1
    CsgDifference s1 s2 -> mkTrace t where
      t pt v = (Seq.filter $ not . within s2) (appTrace (getTrace s1) pt v)
            <> (Seq.filter $ within s1) (appTrace (getTrace s2) pt v) where
        newPt dist = pt .+^ v ^* dist
        within prim = inquire prim . newPt

instance Num n => EllipsoidLike (CSG n) where
  sphere = CsgEllipsoid mempty

instance Num n => CuboidLike (CSG n) where
  cube = CsgBox mempty

instance Num n => FrustumLike (CSG n) where
  frustum a b = CsgFrustum a b mempty

union :: CSG n -> CSG n -> CSG n
union a b = CsgUnion [a, b]

intersection :: CSG n -> CSG n -> CSG n
intersection a b = CsgIntersection [a, b]

difference :: CSG n -> CSG n -> CSG n
difference a b = CsgDifference a b

