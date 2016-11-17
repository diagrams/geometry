{-# LANGUAGE CPP                   #-}
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
-- Copyright   :  (c) 2016 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Various three-dimensional shapes.
--
-----------------------------------------------------------------------------

module Geometry.ThreeD.Shapes
  (
    -- * Basic 3D shapes
    Ellipsoid(..)
  , sphere

  , Box(..)
  , cube

  , Frustum(..)
  , frustum
  , cone
  , cylinder

    -- * Constructive solid geometry
  , CSG(..)
  , union
  , intersection
  , difference
  ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
import           Data.Foldable          (foldMap)
#endif
import           Control.Lens           (review, (^.), _1)
import           Data.Semigroup
import           Data.Typeable

import           Geometry.Angle
import           Geometry.Envelope
import           Geometry.Points
import           Geometry.Query
import           Geometry.Space
import           Geometry.Trace
import           Geometry.Transform
-- import           Geometry.Solve.Polynomial
import           Geometry.ThreeD.Types
import           Geometry.ThreeD.Vector

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

-- Ellipsoid -----------------------------------------------------------

data Ellipsoid n = Ellipsoid !(Transformation V3 n)
  deriving Typeable

type instance V (Ellipsoid n) = V3
type instance N (Ellipsoid n) = n

instance (Num n, Ord n) => HasQuery (Ellipsoid n) Any where
  getQuery (Ellipsoid tr) = transform tr $
    Query $ \v -> Any $ quadrance (v .-. origin) <= 1

instance Fractional n => Transformable (Ellipsoid n) where
  transform t1 (Ellipsoid t2) = Ellipsoid (t1 <> t2)

instance OrderedField n => Enveloped (Ellipsoid n) where
  getEnvelope (Ellipsoid tr) = transform tr . mkEnvelope (const 1)

instance OrderedField n => Traced (Ellipsoid n) where
  getTrace (Ellipsoid tr) = transform tr . mkTrace $ \(P p) v -> let
    a  =    v `dot` v
    b  = 2 * (p `dot` v)
    c  =    (p `dot` p) - 1
    in
     -- mkSortedList $ quadForm a b c
     mkSortedList $ error "need quadForm" a b c

-- | A sphere of radius 1 with its center at the origin.
sphere :: Num n => Ellipsoid n
sphere = Ellipsoid mempty

-- Cuboid --------------------------------------------------------------

data Box n = Box !(Transformation V3 n)
  deriving Typeable

type instance V (Box n) = V3
type instance N (Box n) = n

instance Fractional n => Transformable (Box n) where
  transform t1 (Box t2) = Box (t1 <> t2)

instance OrderedField n => Enveloped (Box n) where
  getEnvelope (Box tr) = transform tr . mkEnvelope $ \v ->
    maximum (map (v `dot`) corners) where
      corners = mkR3 <$> [0,1] <*> [0,1] <*> [0,1]

instance (Fractional n, Ord n) => Traced (Box n) where
  getTrace (Box tr) = transform tr . mkTrace $ \p v -> let
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
     -- ts gives all intersections with the planes forming the box
     -- filter keeps only those actually on the box surface
     mkSortedList . filter (range . atT) $ ts where

instance (Num n, Ord n) => HasQuery (Box n) Any where
  getQuery (Box tr) = transform tr . Query $ Any . range where
    range u = and [x >= 0, x <= 1, y >= 0, y <= 1, z >= 0, z <= 1] where
      (x, y, z) = unp3 u

-- | A cube with side length 1, in the positive octant, with one
-- vertex at the origin.
cube :: Num n => Box n
cube = Box mempty

-- Frustum -------------------------------------------------------------

data Frustum n = Frustum n n (Transformation V3 n)
  deriving Typeable

type instance V (Frustum n) = V3
type instance N (Frustum n) = n

instance Fractional n => Transformable (Frustum n) where
  transform t1 (Frustum r0 r1 t2) = Frustum r0 r1 (t1 <> t2)

instance (OrderedField n, RealFloat n) => Enveloped (Frustum n) where
  -- The plane containing v and the z axis intersects the frustum in a trapezoid
  -- Test the four corners of this trapezoid; one must determine the Envelope
  getEnvelope (Frustum r0 r1 tr) = transform tr . mkEnvelope $ \v ->
    let θ       = v ^. _theta
        corners = [(r1,θ,1), (-r1,θ,1), (r0,θ,0), (-r0,θ,0)]
    in  maximum . map (norm . project v . review r3CylindricalIso) $ corners

instance (RealFloat n, Ord n) => Traced (Frustum n) where
  -- The trace can intersect the sides of the cone or one of the end
  -- caps The sides are described by a quadric equation; substitute
  -- in the parametric form of the ray but disregard any
  -- intersections outside z = [0,1] Similarly, find intersections
  -- with the planes z=0, z=1, but disregard any r>r0, r>r1
  getTrace (Frustum r0 r1 tr) = transform tr . mkTrace $ \p v -> let
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
     mkSortedList $ filter zbounds (error "Traced Frustum not yet implimented" a b c) ++ ends

-- | A frustum of a right circular cone.  It has height 1 oriented
-- along the positive z axis, and radii r0 and r1 at Z=0 and Z=1.
-- 'cone' and 'cylinder' are special cases.
frustum :: Num n => n -> n -> Frustum n
frustum r0 r1 = Frustum r0 r1 mempty

-- | A cone with its base centered on the origin, with radius 1 at the
-- base, height 1, and it's apex on the positive Z axis.
cone :: Num n => Frustum n
cone = frustum 1 0

-- | A circular cylinder of radius 1 with one end cap centered on the
-- origin, and extending to Z=1.
cylinder :: Num n => Frustum n
cylinder = frustum 1 1

instance OrderedField n => HasQuery (Frustum n) Any where
  getQuery (Frustum r0 r1 tr) = transform tr $
    Query $ \p -> let
      z = p^._z
      r = r0 + (r1 - r0)*z
      v = p .-. origin
      a = norm $ projectXY v
      projectXY u = u ^-^ project unitZ u
      in
       Any $ z >= 0 && z <= 1 && a <= r

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
  = CsgEllipsoid (Ellipsoid n)
  | CsgBox (Box n)
  | CsgFrustum (Frustum n)
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
    CsgFrustum p        -> CsgFrustum $ transform t p
    CsgUnion ps         -> CsgUnion $ map (transform t) ps
    CsgIntersection ps  -> CsgIntersection $ map (transform t) ps
    CsgDifference p1 p2 -> CsgDifference (transform t p1) (transform t p2)

-- | The Envelope for an Intersection or Difference is simply the
-- Envelope of the Union.  This is wrong but easy to implement.
instance RealFloat n => Enveloped (CSG n) where
  getEnvelope = \case
    CsgEllipsoid p      -> getEnvelope p
    CsgBox p            -> getEnvelope p
    CsgFrustum p        -> getEnvelope p
    CsgUnion ps         -> foldMap getEnvelope ps
    CsgIntersection ps  -> foldMap getEnvelope ps
    CsgDifference p1 p2 -> getEnvelope p1 <> getEnvelope p2
-- TODO after implementing some approximation scheme, calculate
-- correct (approximate) envelopes for intersections and difference.

instance (Floating n, Ord n) => HasQuery (CSG n) Any where
  getQuery = \case
    CsgEllipsoid prim   -> getQuery prim
    CsgBox prim         -> getQuery prim
    CsgFrustum prim     -> getQuery prim
    CsgUnion ps         -> foldMap getQuery ps
    CsgIntersection ps  -> Any . getAll <$> foldMap (fmap (All . getAny) . getQuery) ps
    CsgDifference p1 p2 -> inOut <$> getQuery p1 <*> getQuery p2 where
      inOut (Any a) (Any b) = Any $ a && not b

instance (RealFloat n, Ord n) => Traced (CSG n) where
  getTrace = \case
    CsgEllipsoid p  -> getTrace p
    CsgBox p        -> getTrace p
    CsgFrustum p    -> getTrace p

    -- on surface of some p, and not inside any of the others
    CsgUnion []     -> mempty
    CsgUnion (s:ss) -> mkTrace t where
      t pt v = onSortedList (filter $ without s) (appTrace (getTrace (CsgUnion ss)) pt v)
           <> onSortedList (filter $ without (CsgUnion ss)) (appTrace (getTrace s) pt v) where
        newPt dist = pt .+^ v ^* dist
        without prim = not . inquire prim . newPt

    -- on surface of some p, and inside all the others
    CsgIntersection []     -> mempty
    CsgIntersection (s:ss) -> mkTrace t where
      t pt v = onSortedList (filter $ within s) (appTrace (getTrace (CsgIntersection ss)) pt v)
           <> onSortedList (filter $ within (CsgIntersection ss)) (appTrace (getTrace s) pt v) where
        newPt dist = pt .+^ v ^* dist
        within prim = inquire prim . newPt

    -- on surface of p1, outside p2, or on surface of p2, inside p1
    CsgDifference s1 s2 -> mkTrace t where
      t pt v = onSortedList (filter $ not . within s2) (appTrace (getTrace s1) pt v)
           <> onSortedList (filter $ within s1) (appTrace (getTrace s2) pt v) where
        newPt dist = pt .+^ v ^* dist
        within prim = inquire prim . newPt

-- | Types which can be included in CSG trees.
class CsgPrim a where
  toCsg :: a n -> CSG n

instance CsgPrim Ellipsoid where
  toCsg = CsgEllipsoid

instance CsgPrim Box where
  toCsg = CsgBox

instance CsgPrim Frustum where
  toCsg = CsgFrustum

instance CsgPrim CSG where
  toCsg = id

union :: (CsgPrim a, CsgPrim b) => a n -> b n -> CSG n
union a b = CsgUnion [toCsg a, toCsg b]

intersection :: (CsgPrim a, CsgPrim b) => a n -> b n -> CSG n
intersection a b = CsgIntersection [toCsg a, toCsg b]

difference :: (CsgPrim a, CsgPrim b) => a n -> b n -> CSG n
difference a b = CsgDifference (toCsg a) (toCsg b)

