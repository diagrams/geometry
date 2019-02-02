{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Located
-- Copyright   :  (c) 2013-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- \"Located\" things, /i.e./ things with a concrete location:
-- intuitively, @Located a ~ (a, Point)@.  Wrapping a translationally
-- invariant thing (/e.g./ a 'Segment' or 'Trail') in @Located@ pins
-- it down to a particular location and makes it no longer
-- translationally invariant.
--
-----------------------------------------------------------------------------

module Geometry.Located
  ( Located (..)
  , at
  , viewLoc
  , mapLoc
  , located
  , location

  -- * Internals
  , serializeLocWith
  , deserializeLocWith
  )
  where

import           Control.Lens         (Lens, Lens')
import           Control.DeepSeq      (NFData (..))
import qualified Data.Binary          as Binary
import           Data.Bytes.Get       (MonadGet)
import           Data.Bytes.Put       (MonadPut)
import           Data.Bytes.Serial
import           Data.Hashable
import           Data.Hashable.Lifted
import qualified Data.Serialize       as Cereal
import           Text.Read

import           Data.Functor.Classes
import           Data.Typeable
import           Linear.Affine
import           Linear.Vector

import           Geometry.Envelope
import           Geometry.Juxtapose
import           Geometry.Query
import           Geometry.Space
import           Geometry.Trace
import           Geometry.Transform

import           GHC.Generics         (Generic)

-- | \"Located\" things, /i.e./ things with a concrete location:
--   intuitively, @Located a ~ (Point, a)@.  Wrapping a translationally
--   invariant thing (/e.g./ a 'Segment' or 'Trail') in 'Located' pins
--   it down to a particular location and makes it no longer
--   translationally invariant.
--
--   @Located@ is intentionally abstract.  To construct @Located@
--   values, use 'at'.  To destruct, use 'viewLoc', 'unLoc', or 'loc'.
--   To map, use 'mapLoc'.
--
--   Much of the utility of having a concrete type for the @Located@
--   concept lies in the type class instances we can give it.  The
--   'HasOrigin', 'Transformable', 'HasQuery', 'Enveloped', 'Traced',
--   and 'TrailLike' instances are particularly useful; see the
--   documented instances below for more information.
data Located a = Loc
  { loc   :: !(Point (V a) (N a))
    -- ^ Project out the location of a @Located@ value.
  , unLoc :: !a
    -- ^ Project the value of type @a@ out of a @Located a@, discarding
    --   the location.
  } deriving (Typeable, Generic)

infix 5 `at`
-- | Construct a @Located a@ from a value of type @a@ and a location.
--   @at@ is intended to be used infix, like @x \`at\` origin@.
at :: a -> Point (V a) (N a) -> Located a
at a p = Loc p a
{-# INLINE at #-}

-- | Deconstruct a @Located a@ into a location and a value of type
--   @a@.  @viewLoc@ can be especially useful in conjunction with the
--   @ViewPatterns@ extension.
viewLoc :: Located a -> (Point (V a) (N a), a)
viewLoc (Loc p a) = (p,a)
{-# INLINE viewLoc #-}

-- | A lens onto the location of a 'Located'.
location :: Lens' (Located a) (Point (V a) (N a))
location f (Loc p a) = flip Loc a <$> f p
{-# INLINE location #-}

-- | A lens giving access to the object within a 'Located' wrapper.
located :: SameSpace a b => Lens (Located a) (Located b) a b
located f (Loc p a) = Loc p <$> f a
{-# INLINE located #-}

-- | 'Located' is not a @Functor@, since changing the type could
--   change the type of the associated vector space, in which case the
--   associated location would no longer have the right type. 'mapLoc'
--   has an extra constraint specifying that the vector space must
--   stay the same.
--
--   (Technically, one can say that for every vector space @v@,
--   @Located@ is an endofunctor on the category of types
--   with associated vector space @v@; but that is not covered by the
--   standard @Functor@ class.)
mapLoc :: SameSpace a b => (a -> b) -> Located a -> Located b
mapLoc f (Loc p a) = Loc p (f a)
{-# INLINE mapLoc #-}

deriving instance (Eq   (V a (N a)), Eq a  ) => Eq   (Located a)
deriving instance (Ord  (V a (N a)), Ord a ) => Ord  (Located a)

instance (Show1 (V a), Show (N a), Show a) => Show (Located a) where
  showsPrec d (Loc p a) = showParen (d > 5) $
    showsPrec 6 a . showString " `at` " . showsPrec1 6 p

instance (Read (V a (N a)), Read a) => Read (Located a) where
  readPrec = parens . prec 5 $ do
    a <- readPrec
    Punc "`"   <- lexP
    Ident "at" <- lexP
    Punc "`"   <- lexP
    p <- readPrec
    return (Loc p a)

type instance V (Located a) = V a
type instance N (Located a) = N a

-- | @Located a@ is an instance of @HasOrigin@ whether @a@ is or not.
--   In particular, translating a @Located a@ simply translates the
--   associated point (and does /not/ affect the value of type @a@).
instance (Num (N a), Additive (V a)) => HasOrigin (Located a) where
  moveOriginTo o (Loc p a) = Loc (moveOriginTo o p) a

-- | Applying a transformation @t@ to a @Located a@ results in the
--   transformation being applied to the location, and the /linear/
--   /portion/ of @t@ being applied to the value of type @a@ (/i.e./
--   it is not translated).
instance (InSpace v n a, Foldable v, Transformable a) => Transformable (Located a) where
  transform t@(T t1 t2 _) (Loc p a)
    = Loc (papply t p) (transform (T t1 t2 zero) a)
  {-# INLINE transform #-}

-- | The envelope of a @Located a@ is the envelope of the @a@,
--   translated to the location.
instance Enveloped a => Enveloped (Located a) where
  getEnvelope (Loc p a) = moveTo p (getEnvelope a)
  {-# INLINE getEnvelope #-}

-- | The query of a @Located a@ is the query of the @a@, translated to
--   the location.
instance (Additive (V a), Num (N a), HasQuery a m) => HasQuery (Located a) m where
  getQuery (Loc p a) = moveTo p (getQuery a)
  {-# INLINE getQuery #-}

instance Enveloped a => Juxtaposable (Located a) where
  juxtapose = juxtaposeDefault

-- | The trace of a @Located a@ is the trace of the @a@,
--   translated to the location.
instance (Traced a, Num (N a)) => Traced (Located a) where
  getTrace (Loc p a) = moveTo p (getTrace a)

instance (NFData (Vn a), NFData a) => NFData (Located a) where
  rnf (Loc p a) = rnf p `seq` rnf a
  {-# INLINE rnf #-}

instance (Hashable1 (V a), Hashable (N a), Hashable a) => Hashable (Located a) where
  hashWithSalt s (Loc (P p) a) = hashWithSalt1 s p `hashWithSalt` a
  {-# INLINE hashWithSalt #-}

serializeLocWith
  :: (MonadPut m, Serial1 (V a))
  => (N a -> m ()) -> (a -> m ()) -> Located a -> m ()
serializeLocWith nf af (Loc (P p) a) = do
  serializeWith nf p
  af a
{-# INLINE serializeLocWith #-}

deserializeLocWith
  :: (MonadGet m, Serial1 (V a))
  => m (N a) -> m a -> m (Located a)
deserializeLocWith mn ma = do
  p <- deserializeWith mn
  a <- ma
  return (Loc (P p) a)
{-# INLINE deserializeLocWith #-}

instance (Serial1 (V a), Serial (N a), Serial a) => Serial (Located a) where
  serialize = serializeLocWith serialize serialize
  {-# INLINE serialize #-}
  deserialize = deserializeLocWith deserialize deserialize
  {-# INLINE deserialize #-}

instance (Serial1 (V a), Binary.Binary (N a), Binary.Binary a)
    => Binary.Binary (Located a) where
  put = serializeLocWith Binary.put Binary.put
  {-# INLINE put #-}
  get = deserializeLocWith Binary.get Binary.get
  {-# INLINE get #-}

instance (Serial1 (V a), Cereal.Serialize (N a), Cereal.Serialize a)
    => Cereal.Serialize (Located a) where
  put = serializeLocWith Cereal.put Cereal.put
  {-# INLINE put #-}
  get = deserializeLocWith Cereal.get Cereal.get
  {-# INLINE get #-}

