{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
-- https://ghc.haskell.org/trac/ghc/ticket/14253

-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Path
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
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

    Path(..)

    -- * Constructing paths
    -- $construct

  , ToPath (..)

    -- * Eliminating paths

  , pathPoints
  , pathVertices'
  , pathVertices
  , pathOffsets
  , pathCentroid
  , pathLocSegments, fixPath

  , explodePath

   -- * Modifying paths

  , scalePath
  , partitionPath

  ) where

import           Control.Arrow                      ((***))
import           Control.DeepSeq                    (NFData (..))
import           Control.Lens                       hiding (at, transform,
                                                     ( # ))
import           Control.Monad
import qualified Data.Binary                        as Binary
import           Data.Bytes.Serial
import           Data.Coerce
import qualified Data.Foldable                      as F
import           Data.Functor.Classes
import           Data.Hashable
import           Data.Hashable.Lifted
import qualified Data.Semigroup                     as Sem
import           Data.Sequence                      (Seq)
import qualified Data.Sequence                      as Seq
import qualified Data.Serialize                     as Cereal
import           Data.Typeable
import           GHC.Generics                       (Generic)
import           Numeric.Interval.NonEmpty.Internal hiding (scale)
import           Text.Show                          (showListWith)

import           Linear

import           Geometry.Direction
import           Geometry.Envelope
import           Geometry.Located
import           Geometry.Points
import           Geometry.Query
import           Geometry.Segment
import           Geometry.Space
import           Geometry.Trace
import           Geometry.Trail
import           Geometry.Transform

------------------------------------------------------------------------
-- Paths
------------------------------------------------------------------------

-- | A /path/ is a (possibly empty) sequence of 'Located' 'Trail's.
--   Hence, unlike trails, paths are not translationally invariant,
--   and they form a monoid under /superposition/ (placing one path on
--   top of another) rather than concatenation.
newtype Path v n = Path (Seq (Located (Trail v n)))
  deriving (Sem.Semigroup, Monoid, Generic, Typeable, Eq)

_Path :: Iso (Path v n) (Path v' n') (Seq (Located (Trail v n))) (Seq (Located (Trail v' n')))
_Path = coerced

-- instance (OrderedField n, Metric v, Serialize (v n), Serialize (V n (N n))) =>
-- instance (OrderedField n, Metric v, Serialize (v n), Serialize (V (v n) (N (v n)))) =>
--   Serialize (Path v n)

instance Rewrapped (Path v n) (Path v' n')
instance Wrapped (Path v n) where
  type Unwrapped (Path v n) = [Located (Trail v n)]
  _Wrapped' = _Path . _Wrapped'
  {-# INLINE _Wrapped' #-}

instance Each (Path v n) (Path v' n') (Located (Trail v n)) (Located (Trail v' n')) where
  each = _Path . traversed
  {-# INLINE each #-}

instance AsEmpty (Path v n) where
  _Empty = _Path . _Empty
  {-# INLINE _Empty #-}

instance Cons (Path v n) (Path v' n') (Located (Trail v n)) (Located (Trail v' n')) where
  _Cons = _Path . _Cons . bimapping id (from _Path)
  {-# INLINE _Cons #-}

instance Snoc (Path v n) (Path v' n') (Located (Trail v n)) (Located (Trail v' n')) where
  _Snoc = _Path . _Snoc . bimapping (from _Path) id
  {-# INLINE _Snoc #-}

instance Show1 v => Show1 (Path v) where
  liftShowsPrec x y d (Path ts) = showParen (d > 10) $
    showString "toPath " . showListWith f (F.toList ts)
    where
    f (Loc p a) = liftShowsPrec x y 6 a . showString " `at` " . liftShowsPrec x y 6 p

instance (Show1 v, Show n) => Show (Path v n) where
  showsPrec = showsPrec1

type instance V (Path v n) = v
type instance N (Path v n) = n

instance (Additive v, Num n) => HasOrigin (Path v n) where
  moveOriginTo = over _Path . fmap . moveOriginTo

-- | Paths are trail-like; a located trail can be used to construct a
--   singleton path.
instance FromTrail (Path v n) where
  fromLocTrail = coerce (Seq.singleton :: Located (Trail v n) -> Seq (Located (Trail v n)))
  {-# INLINE fromLocTrail #-}

instance (HasLinearMap v, Metric v, OrderedField n) => Transformable (Path v n) where
  transform = coerce (fmap . transform :: Transformation v n -> Seq (Located (Trail v n)) -> Seq (Located (Trail v n)))
  {-# INLINE transform #-}

locTrailEnv :: (Metric v, OrderedField n) => Located (Trail v n) -> v n -> Interval n
locTrailEnv (Loc p t) v = shift (trailEnv t v)
  where
    shift (I a b) = I (a + x) (b + x)
    x = v `dot` (p^._Point)
{-# INLINE locTrailEnv #-}

pathEnv :: (Metric v, OrderedField n) => Interval n -> Path v n -> v n -> Interval n
pathEnv i0 (Path ts) = \v -> F.foldl' (\i t -> hull (locTrailEnv t v) i) i0 ts
{-# INLINE pathEnv #-}

pathEnvelope :: (Metric v, OrderedField n) => Path v n -> Envelope v n
pathEnvelope = \case
  Path (t :< ts) -> Envelope $ \(Dir v) -> pathEnv (locTrailEnv t v) (Path ts) v
  _              -> EmptyEnvelope
-- This could be defined as @foldMap getEnvelope ts@ but this would be
-- less efficient.
{-# SPECIALISE pathEnvelope :: Path V2 Double -> Envelope V2 Double #-}
{-# SPECIALISE pathEnvelope :: Path V3 Double -> Envelope V3 Double #-}

instance (Metric v, OrderedField n) => Enveloped (Path v n) where
  getEnvelope = pathEnvelope
  {-# INLINE getEnvelope #-}

pathTrace :: OrderedField n => Path V2 n -> Point V2 n -> V2 n -> Seq n
pathTrace = \(Path ts) p0 v ->
  F.foldMap (\(Loc (P p) l) -> trailTrace l (p0 .-^ p) v) ts
{-# SPECIALISE pathTrace :: Path V2 Double -> Point V2 Double -> V2 Double -> Seq Double #-}

instance OrderedField n => Traced (Path V2 n) where
  getTrace = \path -> Trace (pathTrace path)
  {-# INLINE getTrace #-}

instance OrderedField n => HasQuery (Path V2 n) Crossings where
  getQuery = F.foldMap getQuery . view _Path
  {-# INLINE getQuery #-}

instance NFData (v n) => NFData (Path v n) where
  rnf (Path ts) = rnf ts
  {-# INLINE rnf #-}

instance (Hashable1 v, Hashable n) => Hashable (Path v n) where
  hashWithSalt s0 (Path ts) =
    F.foldl' (\s a -> hashWithSalt s a) s0 ts
      `hashWithSalt` Seq.length ts
  {-# INLINE hashWithSalt #-}

instance Serial1 v => Serial1 (Path v) where
  serializeWith f (Path ts) = serializeWith serialLocTrail ts
    where
      serialLocTrail = serializeLocWith f serialTrail
      serialTrail    = serializeWith f
  {-# INLINE serializeWith #-}
  deserializeWith m = Path `liftM` deserializeWith deserialLocTrail
    where
      deserialLocTrail = deserializeLocWith m deserialTrail
      deserialTrail    = deserializeWith m
  {-# INLINE deserializeWith #-}

instance (Serial1 v, Serial n) => Serial (Path v n) where
  serialize = serialize1
  {-# INLINE serialize #-}
  deserialize = deserialize1
  {-# INLINE deserialize #-}

instance (Serial1 v, Binary.Binary n) => Binary.Binary (Path v n) where
  put = serializeWith Binary.put
  {-# INLINE put #-}
  get = deserializeWith Binary.get
  {-# INLINE get #-}

instance (Serial1 v, Cereal.Serialize n) => Cereal.Serialize (Path v n) where
  put = serializeWith Cereal.put
  {-# INLINE put #-}
  get = deserializeWith Cereal.get
  {-# INLINE get #-}

-- instance (Metric v, OrderedField n) => Juxtaposable (Path v n) where
--   juxtapose = juxtaposeDefault

------------------------------------------------------------------------
-- Constructing paths
------------------------------------------------------------------------

-- | Type class for things that can be converted to a 'Path'.
--
--   Note that this class is different from 'FromTrail'. 'FromTrail'
--   is usually the result of a library function to give you a
--   convenient, polymorphic *result* ('Path', 'Diagram'
--   etc.). 'ToPath' takes a polymorphic *input* and converts it to a
--   'Path'.  There is necessarily some overlap in utility (for
--   example, to convert a 'Trail' to a 'Path' one could equivalently
--   use either 'fromTrail' or 'toPath').
--
--   Instances include paths, trails, lines, loops, segments and fixed
--   segments, 'Located' variants of these, and lists of things that
--   can be converted to a path.
--
class ToPath t where
  -- | Convert something to a 'Path'.
  toPath :: (Metric (V t), OrderedField (N t)) => t -> Path (V t) (N t)

instance ToPath (Path v n) where
  toPath = id

instance ToPath (Trail v n) where
  toPath = fromTrail
  {-# INLINE toPath #-}

instance ToPath (Line v n) where
  toPath = fromLine
  {-# INLINE toPath #-}

instance ToPath (Loop v n) where
  toPath = fromLoop
  {-# INLINE toPath #-}

instance ToPath (Located (Trail v n)) where
  toPath = fromLocTrail
  {-# INLINE toPath #-}

instance ToPath (Located (Line v n)) where
  toPath = fromLocLine
  {-# INLINE toPath #-}

instance ToPath (Located (Loop v n)) where
  toPath = fromLocLoop
  {-# INLINE toPath #-}

instance ToPath (Located (Segment v n)) where
  toPath (viewLoc -> (p,seg)) = Path $ Seq.singleton (fromSegments [seg] `at` p)

instance ToPath (Located [Segment v n]) where
  toPath (viewLoc -> (p,segs)) = Path $ Seq.singleton (fromSegments segs `at` p)

instance ToPath (FixedSegment v n) where
  toPath = toPath . view fixed

instance ToPath a => ToPath [a] where
  toPath = F.foldMap toPath

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
pathVertices' toler = map (trailVertices' toler) . view _Wrapped'

-- | Like 'pathVertices'', with a default tolerance.
pathVertices :: (Metric v, OrderedField n) => Path v n -> [[Point v n]]
pathVertices = fmap trailVertices . view _Wrapped'

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
--   This function is not re-exported from "Geometry"; to use
--   it, import "Geometry.Path".
pathPoints :: (Metric v, OrderedField n) => Path v n -> [[Point v n]]
pathPoints = fmap trailPoints . view _Wrapped'

-- | Compute the total offset of each trail comprising a path (see 'trailOffset').
pathOffsets :: (Metric v, OrderedField n) => Path v n -> [v n]
pathOffsets = fmap (offset . unLoc) . view _Wrapped'

-- | Compute the /centroid/ of a path (/i.e./ the average location of
--   its /vertices/; see 'pathVertices').
pathCentroid :: (Metric v, OrderedField n) => Path v n -> Point v n
pathCentroid = meanV . concat . pathVertices

meanV :: (Foldable f, Additive v, Fractional a) => f (v a) -> v a
meanV = uncurry (^/) . F.foldl' (\(s,c) e -> (e ^+^ s,c+1)) (zero,0)
{-# INLINE meanV #-}

-- | Convert a path into a list of lists of located segments.
pathLocSegments :: (Metric v, OrderedField n) => Path v n -> [[Located (Segment v n)]]
pathLocSegments = fmap trailLocSegments . view _Wrapped

-- | Convert a path into a list of lists of 'FixedSegment's.
fixPath :: (Metric v, OrderedField n) => Path v n -> [[FixedSegment v n]]
fixPath = fmap fixTrail . view _Wrapped

-- | \"Explode\" a path by exploding every component trail (see
--   'explodeTrail').
explodePath :: (InSpace v n t, Metric v, OrderedField n, FromTrail t) => Path v n -> [[t]]
explodePath = map explodeTrail . view _Wrapped

-- | Partition a path into two paths based on a predicate on trails:
--   the first containing all the trails for which the predicate returns
--   @True@, and the second containing the remaining trails.
partitionPath :: (Located (Trail v n) -> Bool) -> Path v n -> (Path v n, Path v n)
partitionPath p = (review _Path *** review _Path) . Seq.partition p . view _Path

------------------------------------------------------------
--  Modifying paths  ---------------------------------------
------------------------------------------------------------

-- | Scale a path using its centroid (see 'pathCentroid') as the base
--   point for the scale.
scalePath :: (HasLinearMap v, OrderedField n) => n -> Path v n -> Path v n
scalePath d p = under (movedFrom (pathCentroid p)) (scale d) p

-- | Same as 'reversePath'.
instance (Metric v, OrderedField n) => Reversing (Path v n) where
  reversing = _Path . mapped %~ reversing

