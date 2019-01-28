{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.BoundingBox
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Bounding boxes are not very compositional (/e.g./ it is not
-- possible to do anything sensible with them under rotation), so they
-- are not a good choice for a fundamental simplified representation
-- of an object's geometry.  However, they do have their uses; this
-- module provides definitions and functions for working with them.
-- In particular it is very fast to query whether a given point is
-- contained in a bounding box (/e.g./ using the
-- 'Geometry.Query.inquire' function).
--
-----------------------------------------------------------------------------

module Geometry.BoundingBox
  ( -- * Bounding boxes
    BoundingBox (..)

    -- * Constructing bounding boxes
  , emptyBox, fromCorners, fromPoint, fromPoints

    -- * Queries on bounding boxes
  , isEmptyBox
  , getCorners, getAllCorners
  , boxExtents, boxCenter
  , boxTransform

  , boxContains, boxContains'
  , insideBox, insideBox', outsideBox, outsideBox'

    -- * Operations on bounding boxes
  , boxUnion, boxIntersection
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Coerce
import           Data.Foldable        as F
import           Data.Functor.Classes
import           Data.Maybe           (fromMaybe)
import           Data.Semigroup
import qualified Data.Sequence        as Seq
import           Text.Read

import           Data.Traversable     as T
import           Linear.Affine
import           Linear.Vector

import           Geometry.Query
import           Geometry.Space
import           Geometry.Trace
import           Geometry.Transform

-- | A bounding box is an axis-aligned region determined by two points
--   indicating its \"lower\" and \"upper\" corners. It can also
--   represent an empty bounding box.
--
--   The 'Semigroup' and 'Monoid' instances of 'BoundingBox' can be
--   used to take the union of bounding boxes, with the empty bounding
--   box as the identity.
data BoundingBox v n
  = EmptyBox
  | BoundingBox !(Point v n) !(Point v n)
    -- Invariant: the first point is coordinatewise <= the second point.

type instance V (BoundingBox v n) = v
type instance N (BoundingBox v n) = n

instance (Eq1 v, Eq n) => Eq (BoundingBox v n) where
  EmptyBox          == EmptyBox          = True
  BoundingBox a1 b1 == BoundingBox a2 b2 = eq1 a1 a2 && eq1 b1 b2
  _                 == _                 = False
  {-# INLINE (==) #-}

-- | The combination of two bounding boxes is the smallest bounding
--   box that contains both.
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

instance AsEmpty (BoundingBox v n) where
  _Empty = nearly emptyBox isEmptyBox
  {-# INLINE _Empty #-}

-- A traversal over the two defining corners (pointwise min and max)
-- of a bounding box.  This is an unexported internal utility; it
-- should *not* be exported because it would allow making arbitrary
-- modifications to the box's corners, which could invalidate the
-- invariant that the first corner is coordinatewise <= the second.
boxPoints :: Traversal' (BoundingBox v n) (Point v n)
boxPoints f (BoundingBox a b) = BoundingBox <$> f a <*> f b
boxPoints _ eb                = pure eb
{-# INLINE boxPoints #-}

instance (Additive v, Num n) => HasOrigin (BoundingBox v n) where
  moveOriginTo p = boxPoints %~ moveOriginTo p
  {-# INLINE moveOriginTo #-}

instance (Additive v, Foldable v, Ord n) => HasQuery (BoundingBox v n) Any where
  getQuery = coerce (boxContains :: BoundingBox v n -> Point v n -> Bool)
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
      Range a b -> Seq.fromList [a,b]
      Two a b   -> Seq.fromList [a,b]
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
--   This is a specialised version of 'Empty'.
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

-- | Create a degenerate bounding \"box\" containing only a single
--   point. This is a specialised version of
--   'Geometry.Envelope.boundingBox'.
fromPoint :: Point v n -> BoundingBox v n
fromPoint p = BoundingBox p p
{-# INLINE fromPoint #-}

-- | Create the smallest bounding box containing all the given points.
--   This is a specialised version of 'Geometry.Envelope.boundingBox'.
fromPoints :: (Additive v, Ord n) => [Point v n] -> BoundingBox v n
fromPoints = mconcat . map fromPoint
{-# INLINE fromPoints #-}

-- | Test whether the BoundingBox is empty.
isEmptyBox :: BoundingBox v n -> Bool
isEmptyBox = \case EmptyBox -> True; _ -> False
{-# INLINE isEmptyBox #-}

-- | Get the lower and upper corners that define the bounding box.
getCorners :: BoundingBox v n -> Maybe (Point v n, Point v n)
getCorners (BoundingBox l u) = Just (l, u)
getCorners _                 = Nothing
{-# INLINE getCorners #-}

-- | List all of the corners of the bounding box.
getAllCorners :: (Additive v, Traversable v) => BoundingBox v n -> [Point v n]
getAllCorners EmptyBox          = []
getAllCorners (BoundingBox l u) = T.sequence (liftI2 (\a b -> [a,b]) l u)
{-# INLINE getAllCorners #-}

-- | Get the size of the bounding box, that is, the vector from the (component-wise)
--   smallest corner to the greatest corner. An empty bounding box has 'zero'
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
  let vec = liftU2 (/) (boxExtents v) (boxExtents u)
      T m m_ _  = scalingV vec
  return $ T m m_ (vl ^-^ liftU2 (*) vec ul)

-- | Check whether a point is contained in a bounding box (inclusive
--   of its boundary). This is a specialised version of 'inquire'.
boxContains :: (Additive v, Foldable v, Ord n) => BoundingBox v n -> Point v n -> Bool
boxContains b p = maybe False test $ getCorners b
  where
    test (l, h) = F.and (liftI2 (<=) l p)
               && F.and (liftI2 (<=) p h)

-- | Check whether a point is /strictly/ contained in a bounding box,
--   /i.e./ excluding the boundary.
boxContains' :: (Additive v, Foldable v, Ord n) => BoundingBox v n -> Point v n -> Bool
boxContains' b p = maybe False test $ getCorners b
  where
    test (l, h) = F.and (liftI2 (<) l p)
               && F.and (liftI2 (<) p h)

-- | Test whether the first bounding box is contained inside
--   the second.
insideBox :: (Additive v, Foldable v, Ord n) => BoundingBox v n -> BoundingBox v n -> Bool
insideBox u v = fromMaybe False $ do
  (ul, uh) <- getCorners u
  (vl, vh) <- getCorners v
  return $ F.and (liftI2 (>=) ul vl)
        && F.and (liftI2 (<=) uh vh)

-- | Test whether the first bounding box is /strictly/ contained
--   inside the second.
insideBox' :: (Additive v, Foldable v, Ord n) => BoundingBox v n -> BoundingBox v n -> Bool
insideBox' u v = fromMaybe False $ do
  (ul, uh) <- getCorners u
  (vl, vh) <- getCorners v
  return $ F.and (liftI2 (>) ul vl)
        && F.and (liftI2 (<) uh vh)

-- | Test whether the first bounding box lies outside the second
--   (although they may intersect in their boundaries).
outsideBox :: (Additive v, Foldable v, Ord n) => BoundingBox v n -> BoundingBox v n -> Bool
outsideBox u v = fromMaybe True $ do
  (ul, uh) <- getCorners u
  (vl, vh) <- getCorners v
  return $ F.or (liftI2 (<=) uh vl)
        || F.or (liftI2 (>=) ul vh)

-- | Test whether the first bounding box lies /strictly/ outside the second
--   (they do not intersect at all).
outsideBox' :: (Additive v, Foldable v, Ord n) => BoundingBox v n -> BoundingBox v n -> Bool
outsideBox' u v = fromMaybe True $ do
  (ul, uh) <- getCorners u
  (vl, vh) <- getCorners v
  return $ F.or (liftI2 (<) uh vl)
        || F.or (liftI2 (>) ul vh)

-- | Form the largest bounding box contained within the given two
--   bounding boxes, or @Nothing@ if the two bounding boxes do not
--   overlap at all.
boxIntersection
  :: (Additive v, Foldable v, Ord n)
  => BoundingBox v n -> BoundingBox v n -> BoundingBox v n
boxIntersection u v = maybe mempty (uncurry fromCorners) $ do
  (ul, uh) <- getCorners u
  (vl, vh) <- getCorners v
  return (liftI2 max ul vl, liftI2 min uh vh)

-- | Form the smallest bounding box containing the given two bounding
--   boxes. This is a specialised version of 'mappend'.
boxUnion :: (Additive v, Ord n) => BoundingBox v n -> BoundingBox v n -> BoundingBox v n
boxUnion = mappend
