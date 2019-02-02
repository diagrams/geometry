{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Type families for identifying associated vector spaces.
--
-----------------------------------------------------------------------------

module Geometry.Space
  ( -- * Type families
    V, N

    -- * Type symomyms
  , Vn
  , InSpace, SameSpace
  , HasLinearMap
  , HasBasis
  , OrderedField
  ) where

import           Control.Monad.ST
import           Data.Functor.Rep
import           Data.HashMap.Lazy
import           Data.IntMap
import           Data.Map
import           Data.Monoid.Coproduct
import           Data.Monoid.Deletable
import           Data.Monoid.Split
import           Data.Semigroup
import           Data.Sequence
import           Data.Set
import           Data.Tree

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

------------------------------------------------------------------------
-- Vector spaces
------------------------------------------------------------------------

-- | Many sorts of objects have an associated vector space in which
--   they \"live\".  The type function @V@ maps from object types to
--   the associated vector space. The resulting vector space has kind
--   @* -> *@ which means it takes another type (representing the type
--   of scalars) and returns a concrete vector type. For example 'V2'
--   has kind @* -> *@ and @V2 Double@ represents a vector.
type family V a :: * -> *

-- Note, to use these instances one often needs a constraint of the form
--   V a ~ V b, etc.
type instance V (a,b)   = V a
type instance V (a,b,c) = V a

type instance V (Point v n)   = v
type instance V (a -> b)      = V b
type instance V [a]           = V a
type instance V (Option a)    = V a
type instance V (Set a)       = V a
type instance V (Seq a)       = V a
type instance V (Map k a)     = V a
type instance V (Tree a)      = V a
type instance V (IntMap a)    = V a
type instance V (HashMap k a) = V a
type instance V (IO a)        = V a
type instance V (ST s a)      = V a

type instance V (Deletable m) = V m
type instance V (Split m)     = V m
type instance V (m :+: n)     = V m

-- | N represents the numeric scalar type used for the vector space of
--   an object.
type family N a :: *

type instance N (a,b)   = N a
type instance N (a,b,c) = N a

type instance N (Point v n)   = n
type instance N (a -> b)      = N b
type instance N [a]           = N a
type instance N (Option a)    = N a
type instance N (Set a)       = N a
type instance N (Seq a)       = N a
type instance N (Map k a)     = N a
type instance N (Tree a)      = N a
type instance N (IntMap a)    = N a
type instance N (HashMap k a) = N a
type instance N (IO a)        = N a
type instance N (ST s a)      = N a

type instance N (Deletable m) = N m
type instance N (Split m)     = N m
type instance N (m :+: n)     = N m

-- | Conveient type alias to retrieve the vector type associated with an
--   object's vector space. This is usually used as @Vn a ~ v n@ where @v@ is
--   the vector space and @n@ is the scalar type.
type Vn a = V a (N a)

-- | @InSpace v n a@ means the type @a@ belongs to the vector space @v n@,
--   where @v@ is 'Additive' and @n@ is 'Num'.
type InSpace v n a = (V a ~ v, N a ~ n, Additive v, Num n)

-- | @SameSpace a b@ means the types @a@ and @b@ belong to the same
--   vector space @v n@.
type SameSpace a b = (V a ~ V b, N a ~ N b)

-- Symonyms ------------------------------------------------------------

-- | 'HasLinearMap' is a constraint synonym provided to help shorten
--   some of the ridiculously long constraint sets.
type HasLinearMap v = (Metric v, HasBasis v, Traversable v)

-- | An 'Additive' vector space whose representation is made up of basis elements.
type HasBasis v = (Additive v, Representable v, Rep v ~ E v)

-- | When dealing with envelopes we often want scalars to be an
--   ordered field (i.e. support all four arithmetic operations and be
--   totally ordered) so we introduce this class as a convenient
--   shorthand.
type OrderedField s = (Floating s, Ord s)
