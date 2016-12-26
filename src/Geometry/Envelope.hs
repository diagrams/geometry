{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Geometry.Envelope
-- Copyright   :  (c) 2016 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- diagrams-core defines the core library of primitives forming the
-- basis of an embedded domain-specific language for describing and
-- rendering diagrams.
--
-- The @Diagrams.Core.Envelope@ module defines a data type and type class for
-- \"envelopes\", aka functional bounding regions.
--
-----------------------------------------------------------------------------

module Geometry.Envelope
  ( -- * Envelopes
    Envelope (..)
  , Enveloped (..)

    -- * Utility functions
  , diameter
  -- , radius
  , extent
  , size
  -- , envelopeVMay
  -- , envelopeV
  -- , envelopePMay
  -- , envelopeP
  -- , envelopeSMay
  -- , envelopeS

  ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative                ((<$>))
#endif
import           Control.Lens                       (op)
import qualified Data.Map                           as M
import           Data.Semigroup
import qualified Data.Set                           as S
import           Data.Typeable
import           Linear.Metric
import           Numeric.Interval.NonEmpty.Internal

import           Geometry.BoundingBox
-- import           Geometry.Direction
import           Geometry.HasOrigin
import           Geometry.Points
import           Geometry.Space
import           Geometry.Transform

------------------------------------------------------------------------
-- Envelopes
------------------------------------------------------------------------

-- | Every diagram comes equipped with an /envelope/.  What is an envelope?
--
--   Consider first the idea of a /bounding box/. A bounding box
--   expresses the distance to a bounding plane in every direction
--   parallel to an axis.  That is, a bounding box can be thought of
--   as the intersection of a collection of half-planes, two
--   perpendicular to each axis.
--
--   More generally, the intersection of half-planes in /every/
--   direction would give a tight \"bounding region\", or convex hull.
--   However, representing such a thing intensionally would be
--   impossible; hence bounding boxes are often used as an
--   approximation.
--
--   An envelope is an /extensional/ representation of such a
--   \"bounding region\".  Instead of storing some sort of direct
--   representation, we store a /function/ which takes a direction as
--   input and gives a distance to a bounding half-plane as output.
--   The important point is that envelopes can be composed, and
--   transformed by any affine transformation.
--
--   Formally, given a vector @v@, the envelope computes a scalar @s@ such
--   that
--
--     * for every point @u@ inside the diagram,
--       if the projection of @(u - origin)@ onto @v@ is @s' *^ v@, then @s' <= s@.
--
--     * @s@ is the smallest such scalar.
--
--   There is also a special \"empty envelope\".
--
--   The idea for envelopes came from
--   Sebastian Setzer; see
--   <http://byorgey.wordpress.com/2009/10/28/collecting-attributes/#comment-2030>.  See also Brent Yorgey, /Monoids: Theme and Variations/, published in the 2012 Haskell Symposium: <http://www.cis.upenn.edu/~byorgey/pub/monoid-pearl.pdf>; video: <http://www.youtube.com/watch?v=X-8NCkD2vOw>.
data Envelope v n
  = EmptyEnvelope
  | Envelope (v n -> Interval n)

type instance V (Envelope v n) = v
type instance N (Envelope v n) = n

instance Ord n => Semigroup (Envelope v n) where
  EmptyEnvelope <> e2            = e2
  e1            <> EmptyEnvelope = e1
  Envelope f1   <> Envelope f2   = Envelope $ \v -> hull (f1 v) (f2 v)
  {-# INLINE (<>) #-}

instance Ord n => Monoid (Envelope v n) where
  mappend = (<>)
  {-# INLINE mappend #-}
  mempty = EmptyEnvelope
  {-# INLINE mempty #-}

shift :: Num a => a -> Interval a -> Interval a
shift x (I a b) = I (a + x) (b + x)
{-# INLINE shift #-}

onEnvelope :: ((v n -> Interval n) -> v n -> Interval n) -> Envelope v n -> Envelope v n
onEnvelope _ EmptyEnvelope = EmptyEnvelope
onEnvelope m (Envelope f)  = Envelope (m f)
{-# INLINE onEnvelope #-}

-- | The local origin of an envelope is the point with respect to
--   which bounding queries are made, /i.e./ the point from which the
--   input vectors are taken to originate.
instance (Metric v, Fractional n) => HasOrigin (Envelope v n) where
  moveOriginTo (P u) = onEnvelope (\f v -> shift (negate (u `dot` v)) (f v))
  {-# INLINE moveOriginTo #-}

instance Show (Envelope v n) where
  show EmptyEnvelope = "EmptyEnvelope"
  show _             = "<non-empty envelope>"

instance (Metric v, HasBasis v, Foldable v, Floating n)
    => Transformable (Envelope v n) where
  transform = undefined
  -- transform t = moveOriginTo (P . negated . transl $ t) . onEnvelope g where
  --   g f v = f v' ^/ (v' `dot` vi)
  --     where
  --       v' = signorm $ transp t !* v
  --       vi = apply (inv t) v
  {-# INLINE transform #-}

------------------------------------------------------------------------
--  Enveloped class
------------------------------------------------------------------------

-- | @Enveloped@ abstracts over things which have an envelope.
class (Metric (V a), OrderedField (N a)) => Enveloped a where

  -- | Compute the envelope of an object.  For types with an intrinsic
  --   notion of \"local origin\", the envelope will be based there.
  --   Other types (e.g. 'Trail') may have some other default
  --   reference point at which the envelope will be based; their
  --   instances should document what it is.
  getEnvelope :: a -> Envelope (V a) (N a)

  default getEnvelope :: Foldable f => f a -> Envelope (V a) (N a)
  getEnvelope = foldMap getEnvelope
  {-# INLINE getEnvelope #-}

  boundingBox :: (Typeable (V a), HasBasis (V a), Applicative (V a)) => a -> BoundingBox (V a) (N a)
  boundingBox a =
    case getEnvelope a of
      EmptyEnvelope  -> EmptyBox
      Envelope f     -> BoundingBox (P $ fmap inf bounds) (P $ fmap sup bounds)
        where bounds = fmap f eye
  {-# INLINE boundingBox #-}

instance (Metric v, OrderedField n) => Enveloped (Envelope v n) where
  getEnvelope = id

instance (OrderedField n, Metric v) => Enveloped (Point v n) where
  getEnvelope (P p) = Envelope $ \v -> singleton (p `dot` v)
  {-# INLINE getEnvelope #-}
  boundingBox p = BoundingBox p p
  {-# INLINE boundingBox #-}

instance (Metric v, Traversable v, OrderedField n) => Enveloped (BoundingBox v n) where
  getEnvelope = getEnvelope . getAllCorners
  {-# INLINE getEnvelope #-}
  boundingBox = id
  {-# INLINE boundingBox #-}

instance Enveloped t => Enveloped (TransInv t) where
  getEnvelope = getEnvelope . op TransInv
  {-# INLINE getEnvelope #-}
  boundingBox = boundingBox . op TransInv
  {-# INLINE boundingBox #-}

instance (SameSpace a b, Enveloped a, Enveloped b) => Enveloped (a,b) where
  getEnvelope (x,y) = getEnvelope x <> getEnvelope y
  {-# INLINE getEnvelope #-}
  boundingBox (x,y) = boundingBox x <> boundingBox y
  {-# INLINE boundingBox #-}

instance Enveloped b => Enveloped [b] where
  boundingBox = foldMap boundingBox
  {-# INLINE boundingBox #-}

instance Enveloped b => Enveloped (M.Map k b)
instance Enveloped b => Enveloped (S.Set b)

------------------------------------------------------------------------
--  Computing with envelopes
------------------------------------------------------------------------

-- -- | Compute the vector from the local origin to a separating
-- --   hyperplane in the given direction, or @Nothing@ for the empty
-- --   envelope.
-- envelopeVMay :: Enveloped a => Vn a -> a -> Maybe (Vn a)
-- envelopeVMay v = fmap ((*^ v) . ($ v)) . appEnvelope . getEnvelope
-- {-# INLINE envelopeVMay #-}

-- -- | Compute the vector from the local origin to a separating
-- --   hyperplane in the given direction.  Returns the zero vector for
-- --   the empty envelope.
-- envelopeV :: Enveloped a => Vn a -> a -> Vn a
-- envelopeV v = fromMaybe zero . envelopeVMay v
-- {-# INLINE envelopeV #-}

-- -- | Compute the point on a separating hyperplane in the given
-- --   direction, or @Nothing@ for the empty envelope.
-- envelopePMay :: (InSpace v n a, Enveloped a) => v n -> a -> Maybe (Point v n)
-- envelopePMay v = fmap P . envelopeVMay v
-- {-# INLINE envelopePMay #-}

-- -- | Compute the point on a separating hyperplane in the given
-- --   direction.  Returns the origin for the empty envelope.
-- envelopeP :: (InSpace v n a, Enveloped a) => v n -> a -> Point v n
-- envelopeP v = P . envelopeV v
-- {-# INLINE envelopeP #-}

-- | Compute the diameter of a enveloped object along a particular
--   vector.  Returns zero for the empty envelope.
diameter :: (InSpace v n a, Enveloped a) => v n -> a -> n
diameter v a = maybe 0 (\(lo,hi) -> (hi - lo)) (extent v a)
{-# INLINE diameter #-}

-- | Compute the range of an enveloped object along a certain
--   direction.  Returns a pair of scalars @(lo,hi)@ such that the
--   object extends from @(lo *^ v)@ to @(hi *^ v)@. Returns @Nothing@
--   for objects with an empty envelope.
extent :: (InSpace v n a, Enveloped a) => v n -> a -> Maybe (n, n)
extent v t = case getEnvelope t of
  EmptyEnvelope -> Nothing
  Envelope f    -> let I a b = f v
                   in  Just (a, b)
{-# INLINE extent #-}

-- | The smallest positive vector that bounds the envelope of an object.
size :: (InSpace v n a, Enveloped a, HasBasis v) => a -> v n
size a = fmap (\v -> diameter v a) eye
{-# INLINE size #-}

-- | Get the center of a the bounding box of an enveloped object, return
--   'Nothing' for object with empty envelope.
-- mCenterPoint :: (InSpace v n a, HasBasis v, Enveloped a)
--             => a -> Maybe (Point v n)
-- mCenterPoint = boxCenter . boundingBox

-- | Get the center of a the bounding box of an enveloped object, return
--   the origin for object with empty envelope.
-- centerPoint :: (InSpace v n a, HasBasis v, Enveloped a)
--             => a -> Point v n
-- centerPoint = fromMaybe origin . mCenterPoint

-- | Create a transformation mapping points from one bounding box to the
--   other. Returns 'Nothing' if either of the boxes are empty.
-- boxTransform
--   :: (Additive v, Fractional n)
--   => BoundingBox v n -> BoundingBox v n -> Maybe (Transformation v n)
-- boxTransform u v = do
--   (P ul, _) <- getCorners u
--   (P vl, _) <- getCorners v
--   let -- i  = s (v, u) <-> s (u, v)
--       s = liftU2 (*) . uncurry (liftU2 (/)) . mapT boxExtents
--       m = undefined -- fmap s eye
--   return $ T m m (vl ^-^ s (v, u) ul)

-- | Transforms an enveloped thing to fit within a @BoundingBox@.  If the
--   bounding box is empty, then the result is also @mempty@.
-- boxFit
--   :: (InSpace v n a, HasBasis v, Enveloped a, Transformable a, Monoid a)
--   => BoundingBox v n -> a -> a
-- boxFit b x = maybe mempty (`transform` x) $ boxTransform (boundingBox x) b




