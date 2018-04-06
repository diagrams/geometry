{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Juxtapose
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Things which can be placed \"next to\" other things, for some
-- appropriate notion of \"next to\".
--
-----------------------------------------------------------------------------

module Geometry.Juxtapose
  ( Juxtaposable (..)
  , juxtaposeDefault
  ) where

import qualified Data.Map            as M
import qualified Data.Set            as S

import           Geometry.Envelope
import           Geometry.HasOrigin
import           Geometry.Space

import           Linear.Metric
import           Linear.Vector

-- | Class of things which can be placed \"next to\" other things, for some
--   appropriate notion of \"next to\".
class Juxtaposable a where

  -- | @juxtapose v a1 a2@ positions @a2@ next to @a1@ in the
  --   direction of @v@. In particular, place @a2@ so that @v@ points
  --   from the local origin of @a1@ towards the old local origin of
  --   @a2@; @a1@'s local origin becomes @a2@'s new local origin.  The
  --   result is just a translated version of @a2@.  (In particular,
  --   this operation does not /combine/ @a1@ and @a2@ in any way.)
  juxtapose :: Vn a -> a -> a -> a

-- XXX Is there a reason not to move this into the class with a
-- default method signature specification?

-- | Default implementation of 'juxtapose' for things which are
--   instances of 'Enveloped' and 'HasOrigin'.  If either envelope is
--   empty, the second object is returned unchanged.
juxtaposeDefault :: (Enveloped a, HasOrigin a) => Vn a -> a -> a -> a
juxtaposeDefault = \v a1 a2 ->
  -- the distance a2 needs to be translated such that the hyperplanes between
  -- a1 and a2 are touching

  -- XXX this is not correct given the semantics of extent.  Should
  -- juxtapose take a Direction or a vector?  Or should we normalize?
  let md = do
        (_,d1) <- extent v a1
        (d2,_) <- extent v a2
        Just (d1 - d2)
  in  case md of
        Just d -> moveOriginBy (negate d *^ v) a2
        _      -> a2
{-# INLINE juxtaposeDefault #-}

instance (Metric v, OrderedField n) => Juxtaposable (Envelope v n) where
  juxtapose = juxtaposeDefault

instance (SameSpace a b, Enveloped a, HasOrigin a, Enveloped b, HasOrigin b)
         => Juxtaposable (a,b) where
  juxtapose = juxtaposeDefault

instance (Enveloped b, HasOrigin b) => Juxtaposable [b] where
  juxtapose = juxtaposeDefault

instance (Enveloped b, HasOrigin b) => Juxtaposable (M.Map k b) where
  juxtapose = juxtaposeDefault

instance (Enveloped b, HasOrigin b, Ord b) => Juxtaposable (S.Set b) where
  juxtapose = juxtaposeDefault

instance Juxtaposable b => Juxtaposable (a -> b) where
  juxtapose v f1 f2 b = juxtapose v (f1 b) (f2 b)

