{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.BoundingBox
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Bounding boxes are not very compositional (/e.g./ it is not
-- possible to do anything sensible with them under rotation), so they
-- are not used in the diagrams core.  However, they do have their
-- uses; this module provides definitions and functions for working
-- with them.
--
-----------------------------------------------------------------------------

module Geometry.BoundingBox
  ( -- * Bounding boxes
    BoundingBox (..)

    -- * Constructing bounding boxes
  , emptyBox, fromCorners

    -- * Queries on bounding boxes
  , getCorners, getAllCorners
  , boxExtents, boxCenter
  , boxTransform -- , boxFit
  , boxIntersection
  ) where

-- import           Control.Lens            (AsEmpty (..), Each (..), contains, nearly)
import           Data.Foldable           as F
import           Data.Semigroup
import           Text.Read

import Geometry.Space
import Geometry.Transform
import Geometry.Trace
import           Geometry.Query

import           Control.Applicative
import           Control.Lens hiding (contains, inside, outside)
import Data.Functor.Classes

import           Data.Traversable        as T
import           Linear.Affine
import           Linear.Vector

-- | A bounding box is an axis-aligned region determined by two points
--   indicating its \"lower\" and \"upper\" corners.  It can also represent
--   an empty bounding box - the points are wrapped in @Maybe@.
data BoundingBox v n
  = EmptyBox
  | BoundingBox !(Point v n) !(Point v n)
  deriving Eq

instance (Additive v, Ord n) => Semigroup (BoundingBox v n) where
  EmptyBox <> bb2                        = bb2
  bb1      <> EmptyBox                   = bb1
  BoundingBox a1 b1 <> BoundingBox a2 b2 = BoundingBox (liftU2 min a1 a2) (liftU2 max b1 b2)
  {-# INLINE (<>) #-}
  stimes = stimesIdempotentMonoid

instance (Additive v, Ord n) => Monoid (BoundingBox v n) where
  mappend = (<>)
  {-# INLINE mappend #-}
  mempty = EmptyBox
  {-# INLINE mempty #-}

-- unexported utility
boxPoints :: Traversal' (BoundingBox v n) (Point v n)
boxPoints f (BoundingBox a b) = BoundingBox <$> f a <*> f b
boxPoints _ eb                = pure eb
{-# INLINE boxPoints #-}

instance AsEmpty (BoundingBox v n) where
  _Empty = nearly emptyBox (\case EmptyBox -> True; _ -> False)
  {-# INLINE _Empty #-}

type instance V (BoundingBox v n) = v
type instance N (BoundingBox v n) = n

instance (Additive v, Num n) => HasOrigin (BoundingBox v n) where
  moveOriginTo p = boxPoints %~ moveOriginTo p
  {-# INLINE moveOriginTo #-}

instance (Additive v, Foldable v, Ord n) => HasQuery (BoundingBox v n) Any where
  getQuery EmptyBox          = mempty
  getQuery (BoundingBox l u) = Query $ \p ->
    Any $ F.and (liftI2 (<=) l p) && F.and (liftI2 (<=) p u)
  {-# INLINE getQuery #-}

-- | Possible time values for intersecting a bounding box. Either we
--   intersect for all values of t, two specific values of t or no
--   values of t.
data Intersect a
  = AllT        -- all values may lead to infinite number of intersections
  | MaxTwo      -- all values of t lead two intersections
  | Range !a !a -- intersection for a range of t values
  | Two !a !a   -- two t values for intersections
  | None        -- no t values lead to intersections

instance Ord a => Semigroup (Intersect a) where
  None        <> _           = None
  _           <> None        = None
  AllT        <> a           = allT a
  a           <> AllT        = allT a
  MaxTwo      <> a           = a
  a           <> MaxTwo      = a
  Two a1 b1   <> Two a2 b2   = check Two (max a1 a2) (min b1 b2)
  Range a1 b1 <> Range a2 b2 = check Range (max a1 a2) (min b1 b2)
  Range a1 b1 <> Two a2 b2   = if a1 < a2 && b1 > b2 then Two a2 b2 else None
  Two a1 b1   <> Range a2 b2 = if a2 < a1 && b2 > b1 then Two a1 b1 else None
  {-# INLINE (<>) #-}

check :: Ord a => (a -> a -> Intersect a) -> a -> a -> Intersect a
check f a b = if a <= b then f a b else None
{-# INLINE check #-}

allT :: Intersect a -> Intersect a
allT (Two a b) = Range a b
allT MaxTwo    = MaxTwo
allT a         = a
{-# INLINE allT #-}

instance Ord a => Monoid (Intersect a) where
  mappend = (<>)
  {-# INLINE mappend #-}
  mempty = MaxTwo
  {-# INLINE mempty #-}

bbIntersection
  :: (Additive v, Foldable v, Fractional n, Ord n)
  => BoundingBox v n -> Point v n -> v n -> Intersect n
bbIntersection EmptyBox _ _          = None
bbIntersection (BoundingBox l u) p v = foldr (<>) AllT (liftI4 l u p v)
  where
   -- The near coordinate is the first intersection from coming from
   -- -Infinity *^ v. The far intersection is the first intersection
   -- coming from +Infinity *^ v. The the direcion is negative, the
   -- means the near and far coordinates are flipped.
   -- We return the time where the near and far intersections occur.
   --
   -- a - lower point of bounding box
   -- b - upper point of bounding box
   -- s - trace starting point
   -- d - direction of trace
   f a b s d
     | d == 0    = if s >= a && s <= b then AllT else None
     | otherwise = two ((a-s)/d) ((b-s)/d)

   -- utilities
   liftI4 (P a) (P b) (P c) = liftI2 id (liftI2 id (liftI2 f a b) c)
   {-# INLINE liftI4 #-}

   two :: Ord a => a -> a -> Intersect a
   two a b = if a < b then Two a b else Two b a
   {-# INLINE two #-}

instance (HasLinearMap v, Fractional n, Ord n) => Traced (BoundingBox v n) where
  getTrace bb = mkTrace $ \p v ->
    case bbIntersection bb p v of
      Range a b -> unsafeMkSortedList [a,b]
      Two a b   -> unsafeMkSortedList [a,b]
      _         -> mempty

instance (Show1 v, Show n) => Show (BoundingBox v n) where
  showsPrec d b = case b of
    BoundingBox l u -> showParen (d > 10) $
      showString "fromCorners " . showsPrec1 11 l . showChar ' ' . showsPrec1 11 u
    EmptyBox        -> showString "emptyBox"

instance (Read1 v, Read n) => Read (BoundingBox v n) where
  readPrec = parens $
    (do
      Ident "emptyBox" <- lexP
      pure emptyBox
    ) <|>
    (prec 10 $ do
      Ident "fromCorners" <- lexP
      l <- step (readS_to_Prec readsPrec1)
      h <- step (readS_to_Prec readsPrec1)
      pure $ BoundingBox l h
    )

-- instance Hashable (v n) => Hashable (BoundingBox v n) where
--   hashWithSalt s EmptyBox = s `hashWithSalt` 0
--   hashWithSalt s (BoundingBox l u) = s `hashWithSalt` l `hashWithSalt` u

-- | An empty bounding box.  This is the same thing as @mempty@, but it doesn't
--   require the same type constraints that the @Monoid@ instance does.
emptyBox :: BoundingBox v n
emptyBox = EmptyBox
{-# INLINE emptyBox #-}

-- | Create a bounding box from a point that is component-wise @(<=)@ than the
--   other. If this is not the case, then @mempty@ is returned.
fromCorners
  :: (Additive v, Foldable v, Ord n)
  => Point v n -> Point v n -> BoundingBox v n
fromCorners l h
  | F.and (liftI2 (<=) l h) = BoundingBox l h
  | otherwise               = EmptyBox
{-# INLINE fromCorners #-}

-- | Gets the lower and upper corners that define the bounding box.
getCorners :: BoundingBox v n -> Maybe (Point v n, Point v n)
getCorners (BoundingBox l u) = Just (l, u)
getCorners _                 = Nothing
{-# INLINE getCorners #-}

-- | Computes all of the corners of the bounding box.
getAllCorners :: (Additive v, Traversable v) => BoundingBox v n -> [Point v n]
getAllCorners EmptyBox          = []
getAllCorners (BoundingBox l u) = T.sequence (liftI2 (\a b -> [a,b]) l u)
{-# INLINE getAllCorners #-}

-- | Get the size of the bounding box - the vector from the (component-wise)
--   lesser point to the greater point. An empty bounding box has 'zero'
--   extent.
boxExtents :: (Additive v, Num n) => BoundingBox v n -> v n
boxExtents (BoundingBox l u) = u .-. l
boxExtents _                 = zero

-- | Get the center point in a bounding box.
boxCenter :: (Additive v, Fractional n) => BoundingBox v n -> Maybe (Point v n)
boxCenter = fmap (uncurry (lerp 0.5)) . getCorners
{-# INLINE boxCenter #-}

-- | Create a transformation mapping points from the first bounding box to the
--   second. Returns 'Nothing' if either of the boxes are empty.
boxTransform
  :: (HasLinearMap v, Fractional n)
  => BoundingBox v n -> BoundingBox v n -> Maybe (Transformation v n)
boxTransform u v = do
  (P ul, _) <- getCorners u
  (P vl, _) <- getCorners v
  let -- i  = s (v, u) <-> s (u, v)
      vec = liftU2 (/) (boxExtents v) (boxExtents u)
      -- s = liftU2 (*) . uncurry (liftU2 (/)) . over both boxExtents
      T m m_ _  = scalingV vec
  return $ T m m_ (vl ^-^ liftU2 (*) vec ul)
  -- XXX IS THIS CORRECT?

-- | Form the largest bounding box contained within this given two
--   bounding boxes, or @Nothing@ if the two bounding boxes do not
--   overlap at all.
boxIntersection
  :: (Additive v, Foldable v, Ord n)
  => BoundingBox v n -> BoundingBox v n -> BoundingBox v n
boxIntersection u v = maybe mempty (uncurry fromCorners) $ do
  (ul, uh) <- getCorners u
  (vl, vh) <- getCorners v
  return (liftI2 max ul vl, liftI2 min uh vh)

