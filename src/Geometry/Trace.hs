{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Trace
-- Copyright   :  (c) 2012-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The @Trace@ module defines a data type and type class for
-- \"traces\", aka functional boundaries, essentially corresponding to
-- a raytracer.
--
-----------------------------------------------------------------------------

module Geometry.Trace
  (

    -- * Traces
    Trace(Trace)

  , appTrace
  , mkTrace

    -- * Traced class
  , Traced(..)

    -- * Computing with traces
  , traceV, traceP
  , maxTraceV, maxTraceP
  , getRayTrace
  , rayTraceV, rayTraceP
  , maxRayTraceV, maxRayTraceP

  ) where

import           Control.Lens
import qualified Data.Map           as M
import qualified Data.Semigroup     as Sem
import           Data.Sequence      (Seq)
import qualified Data.Sequence      as Seq
import qualified Data.Set           as S

import           Geometry.HasOrigin
import           Geometry.Space
import           Geometry.Transform

import           Linear.Affine
import           Linear.Vector

------------------------------------------------------------------------
-- Trace
------------------------------------------------------------------------

-- > traceEx = mkTraceDia def

-- | A trace for a given object is like a raytracer: given a ray
--   (represented as a base point and a direction vector), the trace
--   computes a list of signed distances from the base point to all
--   intersections of the ray with the boundary of the object.
--
--   Note that the outputs are not absolute distances, but multipliers
--   relative to the input vector.  That is, if the base point is @p@
--   and direction vector is @v@, and one of the output scalars is
--   @s@, then there is an intersection at the point @p .+^ (s *^ v)@.
--
--   <<diagrams/src_Diagrams_Core_Trace_traceEx.svg#diagram=traceEx&width=200>>
--
--   Traces form a semigroup with pointwise minimum as composition.

newtype Trace v n = Trace { appTrace :: Point v n -> v n -> Seq n }
  deriving (Sem.Semigroup, Monoid)

type instance V (Trace v n) = v
type instance N (Trace v n) = n

instance Rewrapped (Trace v n) (Trace v' n')
instance Wrapped (Trace v n) where
  type Unwrapped (Trace v n) = Point v n -> v n -> Seq n
  _Wrapped' = iso appTrace Trace
  {-# INLINE _Wrapped' #-}

mkTrace :: (Point v n -> v n -> Seq n) -> Trace v n
mkTrace = Trace
{-# INLINE mkTrace #-}

instance (Additive v, Num n) => HasOrigin (Trace v n) where
  moveOriginTo (P u) = _Wrapping' Trace %~ \f p -> f (p .+^ u)

instance Show (Trace v n) where
  show _ = "<trace>"

instance (Additive v, Foldable v, Num n) => Transformable (Trace v n) where
  transform t = _Wrapped %~ \f p v -> f (papply (inv t) p) (apply (inv t) v)
  {-# INLINE transform #-}

------------------------------------------------------------------------
-- Class
------------------------------------------------------------------------

-- | @Traced@ abstracts over things which have a trace.
--
--   If @a@ is also a 'Semigroup' then 'getTrace' must satisfy the law
--
-- @
-- 'getTrace' (a1 <> a2) = 'getTrace' a1 <> 'getTrace' a2
-- @
class (Additive (V a), Ord (N a)) => Traced a where

  -- | Compute the trace of an object.
  getTrace :: a -> Trace (V a) (N a)

instance (Additive v, Ord n) => Traced (Trace v n) where
  getTrace = id

-- | The trace of a single point is the empty trace, /i.e./ the one
--   which returns no intersection points for every query.  Arguably
--   it should return a single finite distance for vectors aimed
--   directly at the given point, but due to floating-point inaccuracy
--   this is problematic.  Note that the envelope for a single point
--   is /not/ the empty envelope (see "Geometry.Envelope").
instance (Additive v, Ord n) => Traced (Point v n) where
  getTrace = const mempty

instance Traced t => Traced (TransInv t) where
  getTrace = getTrace . op TransInv

instance (Traced a, Traced b, SameSpace a b) => Traced (a,b) where
  getTrace (x,y) = getTrace x Sem.<> getTrace y

instance Traced t => Traced [t] where
  getTrace = mconcat . map getTrace

instance Traced t => Traced (M.Map k t) where
  getTrace = mconcat . map getTrace . M.elems

instance Traced t => Traced (S.Set t) where
  getTrace = mconcat . map getTrace . S.elems

------------------------------------------------------------------------
-- Computing traces
------------------------------------------------------------------------

-- | Compute the vector from the given point @p@ to the \"smallest\"
--   boundary intersection along the given vector @v@.  The
--   \"smallest\" boundary intersection is defined as the one given by
--   @p .+^ (s *^ v)@ for the smallest (most negative) value of
--   @s@. Return @Nothing@ if there is no intersection.  See also
--   'traceP'.
--
--   See also 'rayTraceV' which uses the smallest /positive/
--   intersection, which is often more intuitive behavior.
--
--   <<diagrams/src_Diagrams_Core_Trace_traceVEx.svg#diagram=traceVEx&width=600>>
traceV :: (InSpace v n a, Traced a) => Point v n -> v n -> a -> Maybe (v n)
traceV = \p v a -> fmap (*^ v) . minimumOf folded $ appTrace (getTrace a) p v
{-# INLINE traceV #-}

-- > traceVEx = mkTraceDiasABC def { drawV = True, sFilter = take 1 }


-- | Compute the \"smallest\" boundary point along the line determined
--   by the given point @p@ and vector @v@.  The \"smallest\" boundary
--   point is defined as the one given by @p .+^ (s *^ v)@ for
--   the smallest (most negative) value of @s@. Return @Nothing@ if
--   there is no such boundary point.  See also 'traceV'.
--
--   See also 'rayTraceP' which uses the smallest /positive/
--   intersection, which is often more intuitive behavior.
--
--   <<diagrams/src_Diagrams_Core_Trace_tracePEx.svg#diagram=tracePEx&width=600>>
traceP :: (InSpace v n a, Traced a) => Point v n -> v n -> a -> Maybe (Point v n)
traceP p v a = (p .+^) <$> traceV p v a

-- > tracePEx = mkTraceDiasABC def { sFilter = take 1 }


-- | Like 'traceV', but computes a vector to the \"largest\" boundary
--   point instead of the smallest. (Note, however, the \"largest\"
--   boundary point may still be in the opposite direction from the
--   given vector, if all the boundary points are, as in the third
--   example shown below.)
--
--   <<diagrams/src_Diagrams_Core_Trace_maxTraceVEx.svg#diagram=maxTraceVEx&width=600>>
maxTraceV :: (InSpace v n a, Traced a) => Point v n -> V a n -> a -> Maybe (v n)
maxTraceV p = traceV p . negated
{-# INLINE maxTraceV #-}

-- > maxTraceVEx = mkTraceDiasABC def { drawV = True, sFilter = dropAllBut1 }


-- | Like 'traceP', but computes the \"largest\" boundary point
--   instead of the smallest. (Note, however, the \"largest\" boundary
--   point may still be in the opposite direction from the given
--   vector, if all the boundary points are.)
--
--   <<diagrams/src_Diagrams_Core_Trace_maxTracePEx.svg#diagram=maxTracePEx&width=600>>
maxTraceP :: (InSpace v n a, Traced a) => Point v n -> v n -> a -> Maybe (Point v n)
maxTraceP p v a = (p .+^) <$> maxTraceV p v a
{-# INLINE maxTraceP #-}

-- > maxTracePEx = mkTraceDiasABC def { sFilter = dropAllBut1 }


-- | Get a modified 'Trace' for an object which only returns positive
--   boundary points, /i.e./ those boundary points given by a positive
--   scalar multiple of the direction vector.  Note, this property
--   may be destroyed if the resulting 'Trace' is translated at all.
getRayTrace :: (InSpace v n a, Traced a) => a -> Trace v n
getRayTrace = \a -> Trace $ \p v -> Seq.filter (>=0) $ appTrace (getTrace a) p v
{-# INLINE getRayTrace #-}

-- | Compute the vector from the given point to the closest boundary
--   point of the given object in the given direction, or @Nothing@ if
--   there is no such boundary point (as in the third example
--   below). Note that unlike 'traceV', only /positive/ boundary
--   points are considered, /i.e./ boundary points corresponding to a
--   positive scalar multiple of the direction vector.  This is
--   intuitively the \"usual\" behavior of a raytracer, which only
--   considers intersections \"in front of\" the camera.  Compare the
--   second example diagram below with the second example shown for
--   'traceV'.
--
--   <<diagrams/src_Diagrams_Core_Trace_rayTraceVEx.svg#diagram=rayTraceVEx&width=600>>
rayTraceV :: (InSpace v n a, Traced a) => Point v n -> v n -> a -> Maybe (v n)
rayTraceV = \p v a -> fmap (*^ v) . minimumOf folded $ appTrace (getRayTrace a) p v
{-# INLINE rayTraceV #-}

-- > rayTraceVEx = mkTraceDiasABC def { drawV = True, sFilter = take 1 . filter (>0) }


-- | Compute the boundary point on an object which is closest to the
--   given base point in the given direction, or @Nothing@ if there is
--   no such boundary point. Note that unlike 'traceP', only /positive/
--   boundary points are considered, /i.e./ boundary points
--   corresponding to a positive scalar multiple of the direction
--   vector.  This is intuitively the \"usual\" behavior of a raytracer,
--   which only considers intersection points \"in front of\" the
--   camera.
--
--   <<diagrams/src_Diagrams_Core_Trace_rayTracePEx.svg#diagram=rayTracePEx&width=600>>
rayTraceP :: (InSpace v n a, Traced a)
           => Point v n -> v n -> a -> Maybe (Point v n)
rayTraceP = \p v a -> (p .+^) <$> rayTraceV p v a

-- > rayTracePEx = mkTraceDiasABC def { sFilter = take 1 . filter (>0) }


-- | Like 'rayTraceV', but computes a vector to the \"largest\"
--   boundary point instead of the smallest.  Considers only
--   /positive/ boundary points.
--
--   <<diagrams/src_Diagrams_Core_Trace_maxRayTraceVEx.svg#diagram=maxRayTraceVEx&width=600>>
maxRayTraceV :: (InSpace v n a, Traced a)
              => Point v n -> v n -> a -> Maybe (v n)
maxRayTraceV = \p v a -> fmap (*^ v) . maximumOf folded $ appTrace (getRayTrace a) p v

-- > maxRayTraceVEx = mkTraceDiasABC def { drawV = True, sFilter = dropAllBut1 . filter (>0) }


-- | Like 'rayTraceP', but computes the \"largest\" boundary point
--   instead of the smallest.  Considers only /positive/ boundary
--   points.
--
--   <<diagrams/src_Diagrams_Core_Trace_maxRayTracePEx.svg#diagram=maxRayTracePEx&width=600>>
maxRayTraceP :: (InSpace v n a, Traced a)
              => Point v n -> v n -> a -> Maybe (Point v n)
maxRayTraceP p v a = (p .+^) <$> maxRayTraceV p v a
{-# INLINE maxRayTraceP #-}

-- > maxRayTracePEx = mkTraceDiasABC def { sFilter = dropAllBut1 . filter (>0) }

------------------------------------------------------------
-- Drawing trace diagrams
------------------------------------------------------------

-- > import Data.Default.Class
-- > import Control.Lens ((^.))
-- > import Data.Maybe (fromMaybe)
-- >
-- > thingyT :: Trail V2 Double
-- > thingyT =
-- >   fromOffsets
-- >     [ 3 *^ unitX, 3 *^ unitY, 2 *^ unit_X, 1 *^ unit_Y
-- >     , 1 *^ unitX, 1 *^ unit_Y, 2 *^ unit_X, 1 *^ unit_Y ]
-- >
-- > -- thingy = strokeTrail thingyT
-- > thingy = stroke thingyT
-- >
-- > data TraceDiaOpts
-- >   = TDO { traceShape :: Diagram V2
-- >         , basePt     :: P2 Double
-- >         , dirV       :: V2 Double
-- >         , sFilter    :: [Double] -> [Double]
-- >         , drawV      :: Bool
-- >         }
-- >
-- > instance Default TraceDiaOpts where
-- >   def = TDO { traceShape = thingy
-- >             , basePt     = pointB
-- >             , dirV       = 0.3 ^& 0.5
-- >             , sFilter    = id
-- >             , drawV      = False
-- >             }
-- >
-- > pointA = 1 ^& (-1.5)
-- > pointB = 1 ^& 1.2
-- > pointC = 2.5 ^& 3.5
-- >
-- > dot' = circle 0.05 # lw none
-- >
-- > mkTraceDia :: TraceDiaOpts -> Diagram V2
-- > mkTraceDia tdo = mconcat
-- >   [ mconcat $ map (place (dot' # fc red)) pts
-- >   , if drawV tdo then resultArrow else mempty
-- >   , arrowAt (basePt tdo) (dirV tdo) # lc blue
-- >   , dot' # fc blue # moveTo (basePt tdo)
-- >   , traceLine (basePt tdo) maxPosPt
-- >   , traceLine (basePt tdo) minNegPt
-- >   , traceShape tdo
-- >   ]
-- >   # centerXY # pad 1.1
-- >   where
-- >     ss  = sFilter tdo . getSortedList
-- >         $ appTrace (traceShape tdo ^. trace) (basePt tdo) (dirV tdo)
-- >     pts = map mkPt ss
-- >     mkPt s = basePt tdo .+^ (s *^ dirV tdo)
-- >     maxPosPt = (mkPt <$>) . safeLast $ filter (>0) ss
-- >     minNegPt = (mkPt <$>) . safeHead $ filter (<0) ss
-- >     minPt = (mkPt <$>) . safeHead $ ss
-- >     resultArrow = fromMaybe mempty (arrowBetween (basePt tdo) <$> minPt)
-- >       # lc green
-- >
-- > safeLast [] = Nothing
-- > safeLast xs = Just $ last xs
-- > safeHead [] = Nothing
-- > safeHead (x:_) = Just x
-- > dropAllBut1 [] = []
-- > dropAllBut1 xs = [last xs]
-- >
-- > traceLine _ Nothing = mempty
-- > traceLine p (Just q) = (p ~~ q) # dashingG [0.1,0.1] 0
-- >
-- > mkTraceDias :: [TraceDiaOpts] -> Diagram V2
-- > mkTraceDias = hcat' (with & sep .~ 1) . map mkTraceDia
-- >
-- > mkTraceDiasABC :: TraceDiaOpts -> Diagram V2
-- > mkTraceDiasABC tdo = mkTraceDias (map (\p -> tdo { basePt = p }) [pointA, pointB, pointC])

