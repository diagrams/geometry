{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Query
-- Copyright   :  (c) 2013-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A query is a function that maps points in a vector space to values
-- in some monoid. Queries naturally form a monoid, with two queries
-- being combined pointwise.
--
-----------------------------------------------------------------------------

module Geometry.Query
  ( -- * Queries
    Query(..)
  , HasQuery (..)
  , sample
  , inquire
  , queryPoint
  ) where

import           Data.Monoid

import           Control.Lens
import           Data.Distributive
import           Data.Functor.Rep
import           Data.Profunctor
import qualified Data.Profunctor.Rep   as P
import           Data.Profunctor.Sieve
import qualified Data.Semigroup        as Sem

import           Linear.Affine
import           Linear.Vector

import           Geometry.HasOrigin
import           Geometry.Space
import           Geometry.Transform

------------------------------------------------------------------------
-- Queries
------------------------------------------------------------------------

-- | A query is a function that maps points in a vector space to
--   values in some monoid. Queries naturally form a monoid, with
--   two queries being combined pointwise.
--
--   The idea for annotating diagrams with monoidal queries came from
--   the graphics-drawingcombinators package,
--   <http://hackage.haskell.org/package/graphics-drawingcombinators>.
newtype Query v n m = Query { runQuery :: Point v n -> m }
  deriving (Functor, Applicative, Monad, Sem.Semigroup, Monoid)

instance Distributive (Query v n) where
  distribute a = Query $ \p -> fmap (\(Query q) -> q p) a
  {-# INLINE distribute #-}

instance Representable (Query v n) where
  type Rep (Query v n) = Point v n
  tabulate = Query
  {-# INLINE tabulate #-}
  index    = runQuery
  {-# INLINE index    #-}

instance Functor v => Profunctor (Query v) where
  lmap f (Query q) = Query $ \p -> q (fmap f p)
  {-# INLINE lmap #-}
  rmap = fmap
  {-# INLINE rmap #-}

instance Functor v => Cosieve (Query v) (Point v) where
  cosieve = runQuery
  {-# INLINE cosieve #-}

instance Functor v => Closed (Query v) where
  closed (Query fab) = Query $ \fxa x -> fab (fmap ($ x) fxa)
  {-# INLINE closed #-}

instance Functor v => Costrong (Query v) where
  unfirst (Query f) = Query f'
    where f' fa = b where (b, d) = f ((\a -> (a, d)) <$> fa)
  unsecond (Query f) = Query f'
    where f' fa = b where (d, b) = f ((,) d <$> fa)

instance Functor v => P.Corepresentable (Query v) where
  type Corep (Query v) = Point v
  cotabulate = Query

-- | Setter over the input point of a query.
queryPoint :: Setter (Query v' n' m) (Query v n m) (Point v n) (Point v' n')
queryPoint = sets $ \f (Query q) -> Query $ q . f
{-# INLINE queryPoint #-}

instance Wrapped (Query v n m) where
  type Unwrapped (Query v n m) = Point v n -> m
  _Wrapped' = iso runQuery Query

instance Rewrapped (Query v a m) (Query v' a' m')

type instance V (Query v n m) = v
type instance N (Query v n m) = n

instance (Additive v, Num n) => HasOrigin (Query v n m) where
  moveOriginTo (P u) = queryPoint %~ (.+^ u)
  {-# INLINE moveOriginTo #-}

instance (Additive v, Foldable v, Num n) => Transformable (Query v n m) where
  transform t = queryPoint %~ papply (inv t)
  {-# INLINE transform #-}

-- | Types which can answer a 'Query' about points inside the geometric
--   object.
--
--   If @t@ and @m@ are both a 'Semigroup's, 'getQuery' should satisfy
--
-- @
-- 'getQuery' (t1 <> t2) = 'getQuery' t1 <> 'getQuery' t2
-- @
class HasQuery t m | t -> m where
  -- | Extract the query of an object.
  getQuery :: t -> Query (V t) (N t) m

instance HasQuery (Query v n m) m where
  getQuery = id
  {-# INLINE getQuery #-}

-- | Test if a point is not equal to 'mempty'.
--
-- @
-- 'inquire' :: 'QDiagram' b v n 'Any' -> 'Point' v n -> 'Bool'
-- 'inquire' :: 'Query' v n 'Any'      -> 'Point' v n -> 'Bool'
-- 'inquire' :: 'Geometry.BoundingBox.BoundingBox' v n  -> 'Point' v n -> 'Bool'
-- @
inquire :: HasQuery t Any => t -> Point (V t) (N t) -> Bool
inquire t = getAny . sample t
{-# INLINE inquire #-}

-- | Sample an object's query function at a given point.
--
-- @
-- 'sample' :: 'QDiagram' b v n m -> 'Point' v n -> m
-- 'sample' :: 'Query' v n m      -> 'Point' v n -> m
-- 'sample' :: 'Geometry.BoundingBox.BoundingBox' v n  -> 'Point' v n -> 'Any'
-- 'sample' :: 'Geometry.Path.Path' 'V2' 'Double'   -> 'Point' v n -> 'Geometry.TwoD.Path.Crossings'
-- @
sample :: HasQuery t m => t -> Point (V t) (N t) -> m
sample = runQuery . getQuery
{-# INLINE sample #-}

