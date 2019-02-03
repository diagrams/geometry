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
-- Module      :  Geometry.Envelope
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A data type, type class, and associated utility functions for
-- \"envelopes\", /aka/ functional bounding regions.
--
-----------------------------------------------------------------------------

module Geometry.Envelope
  ( -- * Envelopes
    Envelope (..)
  , Enveloped (..)
  , pointEnvelope
  , onEnvelope

    -- * Utility functions
  , diameter
  -- , radius
  , extent
  , size
  , boxFit

  , centerPoint
  , centerPointMay
  ) where

import           Control.Lens                       (both, op, review, (//~),
                                                     _Just)
import qualified Data.Map                           as M
import           Data.Maybe                         (fromMaybe)
import qualified Data.Semigroup                     as Sem
import qualified Data.Set                           as S
import           Linear.Metric
import           Numeric.Interval.NonEmpty.Internal

import           Geometry.BoundingBox
import           Geometry.Direction
import           Geometry.HasOrigin
import           Geometry.Points
import           Geometry.Space
import           Geometry.Transform
import           Linear

------------------------------------------------------------------------
-- Envelopes
------------------------------------------------------------------------

-- | To understand what an envelope is, consider first the idea of a
--   /bounding box/. A bounding box expresses the distance to a
--   bounding plane in every direction parallel to an axis.  That is,
--   a bounding box can be thought of as the intersection of a
--   collection of half-planes, two perpendicular to each axis.
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
--   input and outputs the extent of the object along the given
--   direction.  The important point is that when represented by
--   functions in this way, envelopes can easily be composed, as well
--   as transformed by any affine transformation.
--
--   Formally, given a direction @d@, the envelope applied to @d@
--   returns a pair of real numbers \( (l,h) \) (represented as an
--   'Interval') representing the extent of the object along the
--   direction @d@.  In particular, \(l\) is the largest value such that
--   for any \(l' < l\), @origin .+^ (l' *^ fromDirection d)@ lies
--   outside the object.  Likewise, \(h\) is the smallest value such
--   that for any \(h' > h\), @origin .+^ (h' *^ fromDirection d)@ lies
--   outside the object.
--
--   There is also a special \"empty envelope\".
--
--   The idea for envelopes came from
--   Sebastian Setzer; see
--   <http://byorgey.wordpress.com/2009/10/28/collecting-attributes/#comment-2030>.
--   See also Brent Yorgey, /Monoids: Theme and Variations/, published
--   in the 2012 Haskell Symposium:
--   <http://www.cis.upenn.edu/~byorgey/pub/monoid-pearl.pdf>; video:
--   <http://www.youtube.com/watch?v=X-8NCkD2vOw>.
data Envelope v n
  = EmptyEnvelope
  | Envelope (Direction v n -> Interval n)

type instance V (Envelope v n) = v
type instance N (Envelope v n) = n

-- | The @Semigroup@ instance for @Envelope@ combines them by taking a
--   pointwise union of intervals (representing the union of the
--   bounding regions).
instance Ord n => Sem.Semigroup (Envelope v n) where
  EmptyEnvelope <> e2            = e2
  e1            <> EmptyEnvelope = e1
  Envelope f1   <> Envelope f2   = Envelope $ \v -> hull (f1 v) (f2 v)
  {-# INLINE (<>) #-}

-- | @mempty@ is the empty envelope.
instance Ord n => Monoid (Envelope v n) where
  mappend = (Sem.<>)
  {-# INLINE mappend #-}
  mempty = EmptyEnvelope
  {-# INLINE mempty #-}

-- | Shift both endpoints of an interval by the same offset.
shift :: Num a => a -> Interval a -> Interval a
shift x (I a b) = I (a + x) (b + x)
{-# INLINE shift #-}

-- | Create an envelope for the given point. This has fewer constraints
--   than using the 'getEnvelope' instance for points.
pointEnvelope :: (Metric v, Fractional n) => Point v n -> Envelope v n
pointEnvelope (P p) = Envelope $ \(Dir v) -> singleton (p `dot` v)
{-# INLINE pointEnvelope #-}

-- | Modify an envelope by directly specifying a modification to its
--   underlying function.
onEnvelope :: ((Direction v n -> Interval n) -> Direction v n -> Interval n) -> Envelope v n -> Envelope v n
onEnvelope _ EmptyEnvelope = EmptyEnvelope
onEnvelope m (Envelope f)  = Envelope (m f)
{-# INLINE onEnvelope #-}

-- | The local origin of an envelope is the point with respect to
--   which bounding queries are made, /i.e./ the point from which the
--   input vectors are taken to originate.
instance (Metric v, Fractional n) => HasOrigin (Envelope v n) where
  moveOriginTo (P u) = onEnvelope (\f (Dir v) -> shift (negate (u `dot` v)) (f (Dir v)))
  {-# INLINE moveOriginTo #-}

-- | Since envelopes contain a function, they can't really be shown in
--   a very useful way, but at least we can distinguish between empty
--   and non-empty envelopes.
instance Show (Envelope v n) where
  show EmptyEnvelope = "EmptyEnvelope"
  show _             = "<non-empty envelope>"

instance (Metric v, HasBasis v, Foldable v, Floating n)
    => Transformable (Envelope v n) where
  transform t = moveOriginTo (P . negated . transl $ t) . onEnvelope g where
    g f (Dir v) = I (mul*a) (mul*b)
      where
        v' = signorm $ transp t !* v
        vi = apply (inv t) v
        I a b = f (Dir v')
        mul   = 1 / (v' `dot` vi)
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
  --
  --   'Foldable' containers have a default implementation of
  --   'getEnvelope' which simply merges together the envelopes of all
  --   its items (/i.e./ @foldMap getEnvelope@).
  getEnvelope :: a -> Envelope (V a) (N a)

  default getEnvelope :: (a ~ f b, Foldable f, Enveloped b, V (f b) ~ V b,  N (f b) ~ N b) => a -> Envelope (V a) (N a)
  getEnvelope = foldMap getEnvelope
  {-# INLINE getEnvelope #-}

  -- | An axis-aligned bounding box can be obtained for an @Enveloped@
  --   object (by querying its envelope along each axis).
  boundingBox :: HasBasis (V a) => a -> BoundingBox (V a) (N a)
  boundingBox a =
    case getEnvelope a of
      EmptyEnvelope  -> EmptyBox
      Envelope f     -> BoundingBox (P $ fmap inf bounds) (P $ fmap sup bounds)
        where bounds = fmap (\v -> f (Dir v)) eye
  {-# INLINE boundingBox #-}

instance (Metric v, OrderedField n) => Enveloped (Envelope v n) where
  getEnvelope = id

instance (OrderedField n, Metric v) => Enveloped (Point v n) where
  getEnvelope (P p) = Envelope $ \(Dir v) -> singleton (p `dot` v)
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
  getEnvelope (x,y) = getEnvelope x Sem.<> getEnvelope y
  {-# INLINE getEnvelope #-}
  boundingBox (x,y) = boundingBox x Sem.<> boundingBox y
  {-# INLINE boundingBox #-}

instance Enveloped b => Enveloped [b] where
  getEnvelope = foldMap getEnvelope
  {-# INLINE getEnvelope #-}
  boundingBox = foldMap boundingBox
  {-# INLINE boundingBox #-}
-- XXX can't we just use the default implementations for the above instance?

instance Enveloped b => Enveloped (M.Map k b)
instance Enveloped b => Enveloped (S.Set b)

-- XXX do we want to explicitly define boundingBox = foldMap
-- boundingBox for the above instances?  Seems like it could be more
-- efficient?

------------------------------------------------------------------------
--  Computing with envelopes
------------------------------------------------------------------------

-- | Compute the diameter of a enveloped object along a particular
--   vector. Returns zero for the empty envelope.
diameter :: (InSpace v n a, Enveloped a) => v n -> a -> n
diameter v a = maybe 0 (\(lo,hi) -> (hi - lo) * norm v) (extent v a)
{-# INLINE diameter #-}

-- | Compute the range of an enveloped object along a certain vector.
--   Returns a pair of scalars @(lo,hi)@ such that the object extends
--   from @(lo *^ v)@ to @(hi *^ v)@.  Returns @Nothing@ for objects
--   with an empty envelope.  See also 'extentDir'.
extent :: (InSpace v n a, Enveloped a) => v n -> a -> Maybe (n, n)
extent v = (_Just . both //~ n) . extentDir (review _Dir (v ^/ n))
  where
    n = norm v
{-# INLINE extent #-}

-- | Compute the range of an enveloped object along a certain
--   direction.
extentDir :: (InSpace v n a, Enveloped a) => Direction v n -> a -> Maybe (n, n)
extentDir d t = case getEnvelope t of
  EmptyEnvelope -> Nothing
  Envelope f    -> let I a b = f d
                   in  Just (a, b)
{-# INLINE extentDir #-}

-- | The smallest positive vector that bounds the envelope of an
--   object along each axis.  That is, the sum of vectors which bound
--   the envelope along each axis.  For example, the @size@ of a
--   radius 1 circle is the vector @(2,2)@.
size :: (InSpace v n a, Enveloped a, HasBasis v) => a -> v n
size a = fmap (\v -> diameter v a) eye
{-# INLINE size #-}

-- | Get the center of the bounding box of an enveloped object, or
--   return 'Nothing' for an object with an empty envelope.
centerPointMay :: (InSpace v n a, HasBasis v, Enveloped a)
            => a -> Maybe (Point v n)
centerPointMay = boxCenter . boundingBox

-- | Get the center of a the bounding box of an enveloped object, or
--   return the origin for an object with an empty envelope.  See also
--   'centerPointMay'.
centerPoint :: (InSpace v n a, HasBasis v, Enveloped a)
            => a -> Point v n
centerPoint = fromMaybe origin . centerPointMay

-- | Transform an enveloped thing to fit within a given @BoundingBox@.  If the
--   bounding box is empty, then the result is also @mempty@.
boxFit
  :: (InSpace v n a, HasLinearMap v, Enveloped a, Transformable a, Monoid a)
  => BoundingBox v n -> a -> a
boxFit b x = maybe mempty (`transform` x) $ boxTransform (boundingBox x) b

