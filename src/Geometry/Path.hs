{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DeriveDataTypeable         #-}
#endif
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Path
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module defines /paths/, which are collections of concretely
-- located 'Trail's.  Many drawing systems (cairo, svg, ...) have a
-- similar notion of \"path\".  Note that paths with multiple trails
-- are necessary for being able to draw /e.g./ filled objects with
-- holes in them.
--
-----------------------------------------------------------------------------

module Geometry.Path
  (

    -- * Paths

    Path(..), pathTrails

    -- * Constructing paths
    -- $construct

  , ToPath (..)
  , pathFromTrail
  , pathFromTrailAt
  , pathFromLocTrail

    -- * Eliminating paths

  , pathPoints
  , pathVertices'
  , pathVertices
  , pathOffsets
  , pathCentroid
  , pathLocSegments, fixPath

    -- * Modifying paths

  , scalePath
  , reversePath

    -- * Miscellaneous

  , explodePath
  , partitionPath

  ) where

import           Control.Arrow        ((***))
import           Control.Lens         hiding ((#), transform, at)
import qualified Data.Foldable        as F
import           Data.List            (partition)
import           Data.Semigroup
import           Data.Typeable
import Data.Functor.Classes

import           Geometry.Space
import           Geometry.Envelope
import           Geometry.Juxtapose
import           Geometry.Located
import           Geometry.Points
import           Geometry.Segment
import           Geometry.Trail
import           Geometry.TrailLike
import           Geometry.Transform

import           Linear.Metric
import           Linear.Vector

import           GHC.Generics (Generic)
-- import           Data.Serialize (Serialize)

------------------------------------------------------------
--  Paths  -------------------------------------------------
------------------------------------------------------------

-- | A /path/ is a (possibly empty) list of 'Located' 'Trail's.
--   Hence, unlike trails, paths are not translationally invariant,
--   and they form a monoid under /superposition/ (placing one path on
--   top of another) rather than concatenation.
newtype Path v n = Path [Located (Trail v n)]
  deriving (Semigroup, Monoid, Generic, Typeable)

-- instance (OrderedField n, Metric v, Serialize (v n), Serialize (V n (N n))) =>
-- instance (OrderedField n, Metric v, Serialize (v n), Serialize (V (v n) (N (v n)))) =>
--   Serialize (Path v n)

instance Wrapped (Path v n) where
  type Unwrapped (Path v n) = [Located (Trail v n)]
  _Wrapped' = iso (\(Path x) -> x) Path

instance Rewrapped (Path v n) (Path v' n')

instance Each (Path v n) (Path v' n') (Located (Trail v n)) (Located (Trail v' n')) where
  each = _Wrapped . traverse

instance AsEmpty (Path v n) where
  _Empty = _Wrapped' . _Empty

instance Cons (Path v n) (Path v' n') (Located (Trail v n)) (Located (Trail v' n')) where
  _Cons = _Wrapped . _Cons . bimapping id _Unwrapped
  {-# INLINE _Cons #-}

instance Snoc (Path v n) (Path v' n') (Located (Trail v n)) (Located (Trail v' n')) where
  _Snoc = _Wrapped . _Snoc . bimapping _Unwrapped id
  {-# INLINE _Snoc #-}

-- | Extract the located trails making up a 'Path'.
pathTrails :: Path v n -> [Located (Trail v n)]
pathTrails = op Path

instance (Show1 v, Show n) => Show (Path v n) where
  showsPrec d (Path ts) = showParen (d > 10) $
    showString "toPath " . showsPrec 11 (F.toList ts)

-- deriving instance Show (v n) => Show (Path v n)
-- deriving instance Eq   (v n) => Eq   (Path v n)
-- deriving instance Ord  (v n) => Ord  (Path v n)

type instance V (Path v n) = v
type instance N (Path v n) = n

instance (Additive v, Num n) => HasOrigin (Path v n) where
  moveOriginTo = over _Wrapped' . map . moveOriginTo

-- | Paths are trail-like; a trail can be used to construct a
--   singleton path.
instance (Metric v, OrderedField n) => TrailLike (Path v n) where
  trailLike = Path . (:[])

-- See Note [Transforming paths]
instance (HasLinearMap v, Metric v, OrderedField n)
    => Transformable (Path v n) where
  transform = over _Wrapped . map . transform

instance (Metric v, OrderedField n) => Enveloped (Path v n) where
  getEnvelope = F.foldMap getEnvelope . op Path

instance (Metric v, OrderedField n) => Juxtaposable (Path v n) where
  juxtapose = juxtaposeDefault

------------------------------------------------------------
--  Constructing paths  ------------------------------------
------------------------------------------------------------

-- | Type class for things that can be converted to a 'Path'.
--
--   Note that this class is very different from 'TrailLike'. 'TrailLike' is
--   usually the result of a library function to give you a convenient,
--   polymorphic result ('Path', 'Diagram' etc.).
--
class ToPath t where
  -- | Convert something to a 'Path'.
  toPath :: (Metric (V t), OrderedField (N t)) => t -> Path (V t) (N t)

instance ToPath (Path v n) where
  toPath = id

instance ToPath (Trail v n) where
  toPath = pathFromTrail

instance ToPath (Trail' l v n) where
  toPath t = Path [Trail t `at` origin]

instance ToPath (Located (Trail v n)) where
  toPath = pathFromLocTrail

instance ToPath (Located (Trail' l v n)) where
  toPath = pathFromLocTrail . mapLoc Trail

instance ToPath (Located (Segment Closed v n)) where
  toPath (viewLoc -> (p,seg))
    = Path [trailFromSegments [seg] `at` p]

instance ToPath (Located [Segment Closed v n]) where
  toPath (viewLoc -> (p,segs))
    = Path [trailFromSegments segs `at` p]

instance ToPath (FixedSegment v n) where
  toPath = toPath . view fixed

instance ToPath a => ToPath [a] where
  toPath = F.foldMap toPath

-- $construct
-- Since paths are 'TrailLike', any function producing a 'TrailLike'
-- can be used to construct a (singleton) path.  The functions in this
-- section are provided for convenience.

-- | Convert a trail to a path beginning at the origin.
pathFromTrail :: (Metric v, OrderedField n) => Trail v n -> Path v n
pathFromTrail = trailLike . (`at` origin)

-- | Convert a trail to a path with a particular starting point.
pathFromTrailAt :: (Metric v, OrderedField n) => Trail v n -> Point v n -> Path v n
pathFromTrailAt t p = trailLike (t `at` p)

-- | Convert a located trail to a singleton path.  This is equivalent
--   to 'trailLike', but provided with a more specific name and type
--   for convenience.
pathFromLocTrail :: (Metric v, OrderedField n) => Located (Trail v n) -> Path v n
pathFromLocTrail = trailLike

------------------------------------------------------------
--  Eliminating paths  -------------------------------------
------------------------------------------------------------

-- | Extract the vertices of a path, resulting in a separate list of
--   vertices for each component trail.  Here a /vertex/ is defined as
--   a non-differentiable point on the trail, /i.e./ a sharp corner.
--   (Vertices are thus a subset of the places where segments join; if
--   you want all joins between segments, see 'pathPoints'.)  The
--   tolerance determines how close the tangents of two segments must be
--   at their endpoints to consider the transition point to be
--   differentiable.  See 'trailVertices' for more information.
pathVertices' :: (Metric v, OrderedField n) => n -> Path v n -> [[Point v n]]
pathVertices' toler = map (trailVertices' toler) . op Path

-- | Like 'pathVertices'', with a default tolerance.
pathVertices :: (Metric v, OrderedField n) => Path v n -> [[Point v n]]
pathVertices = map trailVertices . op Path

-- | Extract the points of a path, resulting in a separate list of
--   points for each component trail.  Here a /point/ is any place
--   where two segments join; see also 'pathVertices' and 'trailPoints'.
--
--   This function allows you "observe" the fact that trails are
--   implemented as lists of segments, which may be problematic if we
--   want to think of trails as parametric vector functions. This also
--   means that the behavior of this function may not be stable under
--   future changes to the implementation of trails and paths.  For an
--   unproblematic version which only yields vertices at which there
--   is a sharp corner, excluding points differentiable points, see
--   'pathVertices'.
--
--   This function is not re-exported from "Diagrams.Prelude"; to use
--   it, import "Diagrams.Path".
pathPoints :: (Metric v, OrderedField n) => Path v n -> [[Point v n]]
pathPoints = map trailPoints . op Path

-- | Compute the total offset of each trail comprising a path (see 'trailOffset').
pathOffsets :: (Metric v, OrderedField n) => Path v n -> [v n]
pathOffsets = map (trailOffset . unLoc) . op Path

-- | Compute the /centroid/ of a path (/i.e./ the average location of
--   its /vertices/; see 'pathVertices').
pathCentroid :: (Metric v, OrderedField n) => Path v n -> Point v n
pathCentroid = meanV . concat . pathVertices

meanV :: (Foldable f, Additive v, Fractional a) => f (v a) -> v a
meanV = uncurry (^/) . F.foldl' (\(s,c) e -> (e ^+^ s,c+1)) (zero,0)
{-# INLINE meanV #-}

-- | Convert a path into a list of lists of located segments.
pathLocSegments :: (Metric v, OrderedField n) => Path v n -> [[Located (Segment Closed v n)]]
pathLocSegments = map trailLocSegments . op Path

-- | Convert a path into a list of lists of 'FixedSegment's.
fixPath :: (Metric v, OrderedField n) => Path v n -> [[FixedSegment v n]]
fixPath = map fixTrail . op Path

-- | \"Explode\" a path by exploding every component trail (see
--   'explodeTrail').
explodePath :: (V t ~ v, N t ~ n, TrailLike t) => Path v n -> [[t]]
explodePath = map explodeTrail . op Path

-- | Partition a path into two paths based on a predicate on trails:
--   the first containing all the trails for which the predicate returns
--   @True@, and the second containing the remaining trails.
partitionPath :: (Located (Trail v n) -> Bool) -> Path v n -> (Path v n, Path v n)
partitionPath p = (view _Unwrapped' *** view _Unwrapped') . partition p . op Path

------------------------------------------------------------
--  Modifying paths  ---------------------------------------
------------------------------------------------------------

-- | Scale a path using its centroid (see 'pathCentroid') as the base
--   point for the scale.
scalePath :: (HasLinearMap v, OrderedField n) => n -> Path v n -> Path v n
scalePath d p = under (movedFrom (pathCentroid p)) (scale d) p

-- | Reverse all the component trails of a path.
reversePath :: (Metric v, OrderedField n) => Path v n -> Path v n
reversePath = _Wrapped . mapped %~ reverseLocTrail

-- | Same as 'reversePath'.
instance (Metric v, OrderedField n) => Reversing (Path v n) where
  reversing = _Wrapped' . mapped %~ reversing
