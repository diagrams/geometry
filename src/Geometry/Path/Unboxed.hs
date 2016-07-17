{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Path.Unboxed
-- Copyright   :  (c) 2016 diagrams team (see LICENSE)
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

module Geometry.Path.Unboxed
  (

    -- * Paths

    UPath(..)
  -- , pathTrails

    -- * Constructing paths
    -- $construct

--   , ToPath (..)
--   , pathFromTrail
--   , pathFromTrailAt
--   , pathFromLocTrail

    -- * Eliminating paths

  -- , pathPoints
  -- , pathVertices'
  -- , pathVertices
  -- , pathOffsets
  -- , pathCentroid
  -- , pathLocSegments, fixPath

    -- * Modifying paths

  -- , scalePath
  -- , reversePath

    -- * Miscellaneous

  -- , explodePath
  -- , partitionPath

  ) where

import qualified Data.Foldable          as F
import           Data.Semigroup
import           Data.Sequence          (Seq)
import qualified Data.Sequence          as Seq
import           Data.Typeable
import           Data.Vector.Unboxed    (Unbox)
import           GHC.Generics           (Generic)
-- import           Data.Serialize (Serialize)

import           Geometry.Envelope
import           Geometry.Located
import           Geometry.Query
import           Geometry.Space
import           Geometry.Trace
import           Geometry.Trail.Unboxed
import           Geometry.TrailLike
import           Geometry.Transform
import           Geometry.TwoD.Path     (Crossings)

import           Linear                 (V2, V3)


------------------------------------------------------------
--  Paths  -------------------------------------------------
------------------------------------------------------------

-- | A /path/ is a (possibly empty) list of 'Located' 'Trail's.
--   Hence, unlike trails, paths are not translationally invariant,
--   and they form a monoid under /superposition/ (placing one path on
--   top of another) rather than concatenation.
newtype UPath v n = UPath (Seq (Located (UTrail v n)))
  deriving (Semigroup, Monoid, Generic, Typeable)

-- instance (OrderedField n, Metric v, Serialize (v n), Serialize (V n (N n))) =>
-- instance (OrderedField n, Metric v, Serialize (v n), Serialize (V (v n) (N (v n)))) =>
--   Serialize (Path v n)

-- instance Wrapped (Path v n) where
--   type Unwrapped (Path v n) = [Located (Trail v n)]
--   _Wrapped' = iso (\(Path x) -> x) Path

-- instance Rewrapped (Path v n) (Path v' n')

-- instance Each (UPath v n) (UPath v' n') (Located (UTrail v n)) (Located (UTrail v' n')) where
--   each = _Wrapped . traversed
--   {-# INLINE each #-}

-- instance AsEmpty (Path v n) where
--   _Empty = _Wrapped' . _Empty

-- instance Cons (Path v n) (Path v' n') (Located (Trail v n)) (Located (Trail v' n')) where
--   _Cons = _Wrapped . _Cons . bimapping id _Unwrapped
--   {-# INLINE _Cons #-}

-- instance Snoc (Path v n) (Path v' n') (Located (Trail v n)) (Located (Trail v' n')) where
--   _Snoc = _Wrapped . _Snoc . bimapping _Unwrapped id
--   {-# INLINE _Snoc #-}

-- | Extract the located trails making up a 'Path'.
-- pathTrails :: Path v n -> [Located (Trail v n)]
-- pathTrails = op Path

-- instance (Show1 v, Show n) => Show (Path v n) where
--   showsPrec d (Path ts) = showParen (d > 10) $
--     showString "toPath " . showsPrec 11 (F.toList ts)

-- deriving instance Show (v n) => Show (Path v n)
-- deriving instance Eq   (v n) => Eq   (Path v n)
-- deriving instance Ord  (v n) => Ord  (Path v n)

type instance V (UPath v n) = v
type instance N (UPath v n) = n

instance Unboxable v n => HasOrigin (UPath v n) where
  moveOriginTo p (UPath s) = UPath $ fmap (moveOriginTo p) s
  -- moveOriginTo = over _Wrapped' . fmap . moveOriginTo


-- | Paths are trail-like; a trail can be used to construct a
--   singleton path.
-- instance (Metric v, OrderedField n) => TrailLike (Path v n) where
--   trailLike = Path . Seq.fromList (:[])

instance Unboxable v n => Transformable (UPath v n) where
  transform t (UPath s) = UPath $ fmap (transform t) s
  -- transform = over _Wrapped . fmap . transform

instance Unboxable v n => Enveloped (UPath v n) where
  getEnvelope (UPath s) = case Seq.viewl s of
    t1 Seq.:< s' -> Envelope $ pathEnv t1 s'
      -- F.foldl' (\n t -> max n (runEnvelope t v)) (runEnvelope t1 v) s'
    Seq.EmptyL   -> EmptyEnvelope
  {-# INLINE [0] getEnvelope #-}

pathEnv :: Unboxable v n => Located (UTrail v n) -> Seq (Located (UTrail v n)) -> v n -> n
pathEnv t1 s v = F.foldl' (\n t -> max n (runEnvelope t v)) (runEnvelope t1 v) s
{-# SPECIALISE pathEnv :: Located (UTrail V2 Double) -> Seq (Located (UTrail V2 Double))
                       -> V2 Double -> Double #-}
{-# SPECIALISE pathEnv :: Located (UTrail V3 Double) -> Seq (Located (UTrail V3 Double))
                       -> V3 Double -> Double #-}

runEnvelope :: (InSpace v n a, Enveloped a) => a -> v n -> n
runEnvelope a v =
  case getEnvelope a of
    Envelope f    -> f v
    EmptyEnvelope -> 0
{-# INLINE runEnvelope #-}

instance Unboxable v n => TrailLike (UPath v n) where
  trailLike = UPath . Seq.singleton . trailLike

instance (Unbox n, OrderedField n) => Traced (UPath V2 n) where
  getTrace (UPath ts) = foldMap getTrace ts

instance (Unbox n, OrderedField n) => HasQuery (UPath V2 n) Crossings where
  getQuery (UPath ts) = foldMap getQuery ts

  -- F.foldMap trailEnvelope . op Path
  --         -- this type signature is necessary to work around an apparent bug in ghc 6.12.1
  --   where trailEnvelope :: Located (Trail v n) -> Envelope v n
  --         trailEnvelope (viewLoc -> (p, t)) = moveOriginTo ((-1) *. p) (getEnvelope t)

-- instance (Metric v, OrderedField n) => Juxtaposable (Path v n) where
--   juxtapose = juxtaposeDefault

-- instance (Metric v, OrderedField n) => Alignable (Path v n) where
--   defaultBoundary = envelopeBoundary

------------------------------------------------------------
--  Constructing paths  ------------------------------------
------------------------------------------------------------

-- -- | Type class for things that can be converted to a 'Path'.
-- --
-- --   Note that this class is very different from 'TrailLike'. 'TrailLike' is
-- --   usually the result of a library function to give you a convenient,
-- --   polymorphic result ('Path', 'Diagram' etc.).
-- --
-- class ToPath t where
--   -- | 'toPath' takes something that can be converted to 'Path' and returns
--   --    the 'Path'.
--   toPath :: (Metric (V t), OrderedField (N t)) => t -> Path (V t) (N t)

-- instance ToPath (Path v n) where
--   toPath = id

-- instance ToPath (Trail v n) where
--   toPath = pathFromTrail

-- instance ToPath (Trail' l v n) where
--   toPath t = Path [Trail t `at` origin]

-- instance ToPath (Located (Trail v n)) where
--   toPath = pathFromLocTrail

-- instance ToPath (Located (Trail' l v n)) where
--   toPath = pathFromLocTrail . mapLoc Trail

-- instance ToPath (Located (Segment Closed v n)) where
--   toPath (viewLoc -> (p,seg))
--     = Path [trailFromSegments [seg] `at` p]

-- instance ToPath (Located [Segment Closed v n]) where
--   toPath (viewLoc -> (p,segs))
--     = Path [trailFromSegments segs `at` p]

-- instance ToPath (FixedSegment v n) where
--   toPath = toPath . view fixed

-- instance ToPath a => ToPath [a] where
--   toPath = F.foldMap toPath

-- $construct
-- Since paths are 'TrailLike', any function producing a 'TrailLike'
-- can be used to construct a (singleton) path.  The functions in this
-- section are provided for convenience.

-- | Convert a trail to a path beginning at the origin.
-- pathFromTrail :: (Metric v, OrderedField n) => Trail v n -> Path v n
-- pathFromTrail = trailLike . (`at` origin)

-- | Convert a trail to a path with a particular starting point.
-- pathFromTrailAt :: (Metric v, OrderedField n) => Trail v n -> Point v n -> Path v n
-- pathFromTrailAt t p = trailLike (t `at` p)

-- | Convert a located trail to a singleton path.  This is equivalent
--   to 'trailLike', but provided with a more specific name and type
--   for convenience.
-- pathFromLocTrail :: (Metric v, OrderedField n) => Located (Trail v n) -> Path v n
-- pathFromLocTrail = trailLike

------------------------------------------------------------
--  Eliminating paths  -------------------------------------
------------------------------------------------------------

-- -- | Extract the vertices of a path, resulting in a separate list of
-- --   vertices for each component trail.  Here a /vertex/ is defined as
-- --   a non-differentiable point on the trail, /i.e./ a sharp corner.
-- --   (Vertices are thus a subset of the places where segments join; if
-- --   you want all joins between segments, see 'pathPoints'.)  The
-- --   tolerance determines how close the tangents of two segments must be
-- --   at their endpoints to consider the transition point to be
-- --   differentiable.  See 'trailVertices' for more information.
-- pathVertices' :: (Metric v, OrderedField n) => n -> Path v n -> [[Point v n]]
-- pathVertices' toler = map (trailVertices' toler) . op Path

-- -- | Like 'pathVertices'', with a default tolerance.
-- pathVertices :: (Metric v, OrderedField n) => Path v n -> [[Point v n]]
-- pathVertices = map trailVertices . op Path

-- -- | Extract the points of a path, resulting in a separate list of
-- --   points for each component trail.  Here a /point/ is any place
-- --   where two segments join; see also 'pathVertices' and 'trailPoints'.
-- --
-- --   This function allows you "observe" the fact that trails are
-- --   implemented as lists of segments, which may be problematic if we
-- --   want to think of trails as parametric vector functions. This also
-- --   means that the behavior of this function may not be stable under
-- --   future changes to the implementation of trails and paths.  For an
-- --   unproblematic version which only yields vertices at which there
-- --   is a sharp corner, excluding points differentiable points, see
-- --   'pathVertices'.
-- --
-- --   This function is not re-exported from "Diagrams.Prelude"; to use
-- --   it, import "Diagrams.Path".
-- pathPoints :: (Metric v, OrderedField n) => Path v n -> [[Point v n]]
-- pathPoints = map trailPoints . op Path

-- -- | Compute the total offset of each trail comprising a path (see 'trailOffset').
-- pathOffsets :: (Metric v, OrderedField n) => Path v n -> [v n]
-- pathOffsets = map (trailOffset . unLoc) . op Path

-- -- | Compute the /centroid/ of a path (/i.e./ the average location of
-- --   its /vertices/; see 'pathVertices').
-- pathCentroid :: (Metric v, OrderedField n) => Path v n -> Point v n
-- pathCentroid = meanV . concat . pathVertices

-- meanV :: (Foldable f, Additive v, Fractional a) => f (v a) -> v a
-- meanV = uncurry (^/) . F.foldl' (\(s,c) e -> (e ^+^ s,c+1)) (zero,0)
-- {-# INLINE meanV #-}

-- -- | Convert a path into a list of lists of located segments.
-- pathLocSegments :: (Metric v, OrderedField n) => Path v n -> [[Located (Segment Closed v n)]]
-- pathLocSegments = map trailLocSegments . op Path

-- -- | Convert a path into a list of lists of 'FixedSegment's.
-- fixPath :: (Metric v, OrderedField n) => Path v n -> [[FixedSegment v n]]
-- fixPath = map fixTrail . op Path

-- -- | \"Explode\" a path by exploding every component trail (see
-- --   'explodeTrail').
-- explodePath :: (V t ~ v, N t ~ n, TrailLike t) => Path v n -> [[t]]
-- explodePath = map explodeTrail . op Path

-- -- | Partition a path into two paths based on a predicate on trails:
-- --   the first containing all the trails for which the predicate returns
-- --   @True@, and the second containing the remaining trails.
-- partitionPath :: (Located (Trail v n) -> Bool) -> Path v n -> (Path v n, Path v n)
-- partitionPath p = (view _Unwrapped' *** view _Unwrapped') . partition p . op Path

-- ------------------------------------------------------------
-- --  Modifying paths  ---------------------------------------
-- ------------------------------------------------------------

-- -- | Scale a path using its centroid (see 'pathCentroid') as the base
-- --   point for the scale.
-- scalePath :: (HasLinearMap v, Metric v, OrderedField n) => n -> Path v n -> Path v n
-- scalePath d p = under (movedFrom (pathCentroid p)) (scale d) p

-- -- | Reverse all the component trails of a path.
-- reversePath :: (Metric v, OrderedField n) => Path v n -> Path v n
-- reversePath = _Wrapped . mapped %~ reverseLocTrail

-- -- | Same as 'reversePath'.
-- instance (Metric v, OrderedField n) => Reversing (Path v n) where
--   reversing = _Wrapped' . mapped %~ reversing

