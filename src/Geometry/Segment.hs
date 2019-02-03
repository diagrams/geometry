{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Segment
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A /segment/ is a translation-invariant, atomic path.  Currently,
-- there are two types: linear (/i.e./ just a straight line to the
-- endpoint) and cubic Bézier curves (/i.e./ a curve to an endpoint
-- with two control points).  This module contains tools for creating
-- and manipulating segments, as well as a definition of segments with
-- a fixed location (useful for backend implementors).
--
-- Generally speaking, casual users should not need this module; the
-- higher-level functionality provided by "Geometry.Trail" and
-- "Geometry.Path" should usually suffice.  However, directly
-- manipulating segments can occasionally be useful.
--
-----------------------------------------------------------------------------

module Geometry.Segment
  (
    -- * Segments
    Segment (..)
  , straight
  , bezier3
  , bézier3

  , HasSegments (..)

  -- * Closing segments
  , ClosingSegment (..)
  , linearClosing
  , cubicClosing
  , closingSegment

  -- * Fixed segments
  , FixedSegment (..)
  , fixed

  -- * Crossings
  , Crossings (..)
  , isInsideWinding
  , isInsideEvenOdd

    -- * Low level
    -- ** Folding
  , segmentEnvelope
  , cubicEnvelope
  , envelopeOf
  , traceOf

    -- ** Segment calculations
  , paramsTangentTo
  , splitAtParams
  , unsafeSplitAtParams
  , secondDerivAtParam
  -- , collinear
  , segmentsEqual
  , segmentCrossings
  , linearCrossings
  , cubicCrossings
  , segmentParametersAtDirection
  , bezierParametersAtDirection
  ) where

import           Control.Applicative                (liftA2)
import           Control.DeepSeq                    (NFData (..))
import           Control.Lens                       hiding (at, transform, (<|),
                                                     (|>))
import           Control.Monad
import qualified Data.Binary                        as Binary
import           Data.Bytes.Get                     (getWord8)
import           Data.Bytes.Put                     (putWord8)
import           Data.Bytes.Serial
import           Data.Functor.Classes
import           Data.Hashable
import           Data.Hashable.Lifted
import           Data.List                          (nub, sort)
import qualified Data.Semigroup                     as Sem
import           Data.Sequence                      (Seq)
import qualified Data.Sequence                      as Seq
import qualified Data.Serialize                     as Cereal
import           GHC.Exts                           (build)
import           Linear.Affine
import           Linear.Metric
import           Linear.V2
import           Linear.V3
import           Linear.Vector
import qualified Numeric.Interval.Kaucher           as K
import           Numeric.Interval.NonEmpty.Internal (Interval (..), singleton)

import           Data.Coerce

import           Diagrams.Solve.Polynomial

import           Geometry.Angle
import           Geometry.Direction
import           Geometry.Envelope
import           Geometry.Located
import           Geometry.Parametric
import           Geometry.Query
import           Geometry.Space
import           Geometry.Transform
import           Geometry.TwoD.Transform

------------------------------------------------------------------------
-- Segments
------------------------------------------------------------------------

-- | The atomic constituents of the concrete representation currently
--   used for trails are /segments/, currently limited to single
--   straight lines or cubic Bézier curves. Segments are
--   /translationally invariant/, that is, they have no particular
--   \"location\" and are unaffected by translations. They are, however,
--   affected by other transformations such as rotations and scales.
data Segment v n
  = Linear !(v n)
  | Cubic !(v n) !(v n) !(v n)
  deriving Functor

type instance V (Segment v n) = v
type instance N (Segment v n) = n
type instance Codomain (Segment v n) = v

------------------------------------------------------------
-- Instances

instance (Eq1 v, Eq n) => Eq (Segment v n) where
  Linear v1      == Linear v2      = eq1 v1 v2
  Cubic a1 b1 c1 == Cubic a2 b2 c2 = eq1 a1 a2 && eq1 b1 b2 && eq1 c1 c2
  _              == _              = False
  {-# INLINE (==) #-}

instance Show1 v => Show1 (Segment v) where
  liftShowsPrec x y d seg = case seg of
    Linear v       -> showParen (d > 10) $
      showString "straight " . liftShowsPrec x y 11 v
    Cubic v1 v2 v3 -> showParen (d > 10) $
      showString "bezier3 " . liftShowsPrec x y 11 v1 . showChar ' '
                            . liftShowsPrec x y 11 v2 . showChar ' '
                            . liftShowsPrec x y 11 v3

instance (Show1 v, Show n) => Show (Segment v n) where
  showsPrec = showsPrec1

instance Each (Segment v n) (Segment v n) (v n) (v n) where
  each f (Linear v)       = Linear <$> f v
  each f (Cubic c1 c2 c3) = Cubic <$> f c1 <*> f c2 <*> f c3
  {-# INLINE each #-}

instance NFData (v n) => NFData (Segment v n) where
  rnf = \case
    Linear v       -> rnf v
    Cubic c1 c2 c3 -> rnf c1 `seq` rnf c2 `seq` rnf c3
  {-# INLINE rnf #-}

instance Hashable1 v => Hashable1 (Segment v) where
  liftHashWithSalt f s = \case
    Linear v       -> hws s0 v
    Cubic c1 c2 c3 -> hws (hws (hws s1 c1) c2) c3
    where
      s0 = hashWithSalt s (0::Int)
      s1 = hashWithSalt s (1::Int)
      hws = liftHashWithSalt f
  {-# INLINE liftHashWithSalt #-}

instance (Hashable1 v, Hashable n) => Hashable (Segment v n) where
  hashWithSalt = hashWithSalt1
  {-# INLINE hashWithSalt #-}

instance Serial1 v => Serial1 (Segment v) where
  serializeWith f = \case
    Linear v       -> putWord8 0 >> fv v
    Cubic c1 c2 c3 -> putWord8 1 >> fv c1 >> fv c2 >> fv c3
    where fv = serializeWith f
  {-# INLINE serializeWith #-}

  deserializeWith m = getWord8 >>= \case
    0 -> Linear `liftM` mv
    _ -> Cubic `liftM` mv `ap` mv `ap` mv
    where mv = deserializeWith m
  {-# INLINE deserializeWith #-}

instance (Serial1 v, Serial n) => Serial (Segment v n) where
  serialize = serializeWith serialize
  {-# INLINE serialize #-}
  deserialize = deserializeWith deserialize
  {-# INLINE deserialize #-}

instance (Serial1 v, Binary.Binary n) => Binary.Binary (Segment v n) where
  put = serializeWith Binary.put
  {-# INLINE put #-}
  get = deserializeWith Binary.get
  {-# INLINE get #-}

instance (Serial1 v, Cereal.Serialize n) => Cereal.Serialize (Segment v n) where
  put = serializeWith Cereal.put
  {-# INLINE put #-}
  get = deserializeWith Cereal.get
  {-# INLINE get #-}

------------------------------------------------------------
-- Smart constructors

-- | @'straight' v@ constructs a translationally invariant linear
--   segment with direction and length given by the vector @v@.
straight :: v n -> Segment v n
straight = Linear
{-# INLINE straight #-}

-- | @bezier3 c1 c2 x@ constructs a translationally invariant cubic
--   Bézier curve where the offsets from the first endpoint to the
--   first and second control point and endpoint are respectively
--   given by @c1@, @c2@, and @x@.
bezier3 :: v n -> v n -> v n -> Segment v n
bezier3 = Cubic
{-# INLINE bezier3 #-}

-- | @bézier3@ is the same as @bezier3@, but with more snobbery.
bézier3 :: v n -> v n -> v n -> Segment v n
bézier3 = Cubic
{-# INLINE bézier3 #-}

-- | Things where the segments can be folded over.
class HasSegments t where

  -- | Fold over the segments in a trail.
  segments :: Fold t (Segment (V t) (N t))

  -- | The offset from the start of the first segment to the end of the
  --   last segment.
  offset :: t -> Vn t
  default offset :: (Additive (V t), Num (N t)) => t -> Vn t
  offset = foldlOf' segments (\off seg -> off ^+^ offset seg) zero

  -- | The number of segments.
  numSegments :: t -> Int
  numSegments = lengthOf segments

instance HasSegments (Segment v n) where
  segments f s = f s
  {-# INLINE segments #-}
  offset (Linear v)    = v
  offset (Cubic _ _ v) = v
  {-# INLINE offset #-}
  numSegments _ = 1
  {-# INLINE numSegments #-}

instance HasSegments a => HasSegments (Located a) where
  segments = located . segments
  {-# INLINE segments #-}
  offset = offset . unLoc
  {-# INLINE offset #-}
  numSegments = numSegments . unLoc
  {-# INLINE numSegments #-}

-- Orphan instance because HasSegments is here, and we depend on Loc
-- here. One option would be to have a HasOffset class in Geo.Loc and
-- HasOffset -> HasSegments.
instance (InSpace v n a, HasSegments a, Reversing a) => Reversing (Located a) where
  reversing = \(Loc p a) -> Loc (p .+^ offset a) (reversing a)
  {-# INLINE reversing #-}

instance (Additive v, Foldable v, Num n) => Transformable (Segment v n) where
  transform !t (Linear v)       = Linear (apply t v)
  transform !t (Cubic v1 v2 v3) = Cubic (apply t v1) (apply t v2) (apply t v3)
  {-# INLINE transform #-}

instance (Additive v, Num n) => Parametric (Segment v n) where
  atParam (Linear x) t       = t *^ x
  atParam (Cubic c1 c2 x2) t =  (3 * t'*t'*t ) *^ c1
                            ^+^ (3 * t'*t *t ) *^ c2
                            ^+^ (    t *t *t ) *^ x2
    where t' = 1-t
  {-# INLINE atParam #-}

instance (Additive v, Num n) => Tangential (Segment v n) where
  Linear v `tangentAtParam` _ = v
  Cubic c1 c2 c3 `tangentAtParam` t
    =  (3*(3*t*t-4*t+1))*^ c1
   ^+^ (3*(2-3*t)*t)    *^ c2
   ^+^ (3*t*t)          *^ c3
  {-# INLINE tangentAtParam #-}

instance (Additive v, Num n) => TangentEndValues (Segment v n) where
  tangentAtStart = \case
    Linear v     -> v
    Cubic c1 _ _ -> 3*^c1
  {-# INLINE tangentAtStart #-}
  tangentAtEnd = \case
    Linear v      -> v
    Cubic _ c2 c3 -> 3*^(c3 ^-^ c2)
  {-# INLINE tangentAtEnd #-}

instance Num n => DomainBounds (Segment v n)

instance (Additive v, Num n) => EndValues (Segment v n) where
  atStart             = const zero
  {-# INLINE atStart #-}
  atEnd (Linear v)    = v
  atEnd (Cubic _ _ v) = v
  {-# INLINE atEnd #-}

instance (Additive v, Fractional n) => Sectionable (Segment v n) where
  splitAtParam (Linear x1) t = (left, right)
    where left  = straight p
          right = straight (x1 ^-^ p)
          p = lerp t x1 zero
  splitAtParam (Cubic c1 c2 x2) t = (left, right)
    where left  = bezier3 a b e
          right = bezier3 (c ^-^ e) (d ^-^ e) (x2 ^-^ e)
          p = lerp t c2 c1
          a = lerp t c1 zero
          b = lerp t p a
          d = lerp t x2 c2
          c = lerp t d p
          e = lerp t c b
  {-# INLINE splitAtParam #-}

  reverseDomain (Linear x1)      = Linear (negated x1)
  reverseDomain (Cubic c1 c2 c3) = Cubic (c2 ^-^ c3) (c1 ^-^ c3) (negated c3)
  {-# INLINE reverseDomain #-}

instance (Additive v, Num n) => Reversing (Segment v n) where
  reversing (Linear x1)      = Linear (negated x1)
  reversing (Cubic c1 c2 c3) = Cubic (c2 ^-^ c3) (c1 ^-^ c3) (negated c3)
  {-# INLINE reversing #-}

instance (Metric v, OrderedField n) => HasArcLength (Segment v n) where

  arcLengthBounded _ (Linear x1) = K.singleton $ norm x1
  arcLengthBounded m s@(Cubic c1 c2 x2)
    | ub - lb < m = K.I lb ub
    | otherwise   = arcLengthBounded (m/2) l + arcLengthBounded (m/2) r
   where (l,r) = s `splitAtParam` 0.5
         ub    = sum (map norm [c1, c2 ^-^ c1, x2 ^-^ c2])
         lb    = norm x2

  arcLengthToParam m s _ | arcLength m s == 0 = 0.5
  arcLengthToParam m s@(Linear {}) len = len / arcLength m s
  arcLengthToParam m s@(Cubic {})  len
    | len `K.member` K.I (-m/2) (m/2) = 0
    | len < 0              = - arcLengthToParam m (fst (splitAtParam s (-1))) (-len)
    | len `K.member` slen  = 1
    | len > K.sup slen     = 2 * arcLengthToParam m (fst (splitAtParam s 2)) len
    | len < K.sup llen     = (*0.5) $ arcLengthToParam m l len
    | otherwise            = (+0.5) . (*0.5)
                           $ arcLengthToParam (9*m/10) r (len - K.midpoint llen)
    where (l,r) = s `splitAtParam` 0.5
          llen  = arcLengthBounded (m/10) l
          slen  = arcLengthBounded m s

-- | The second derivative of the parametric curve \(d^2 S(t) / d t^2\).
--   This is similar to curvature except the curvature is the norm of the
--   second derivative with respect to the segment length \(|d^2 \gamma(s)
--   / d s^2|\).
secondDerivAtParam :: (Additive v, Num n) => Segment v n -> n -> v n
secondDerivAtParam s t = case s of
  Linear _       -> zero
  Cubic c1 c2 c3 -> (6*(3*t-2))*^c1 ^+^ (6-18*t)*^c2 ^+^ (6*t)*^c3

-- It's been a while since I've looked at this, not quite sure why it's
-- commented out. Something to do with PosInf I think.

-- curvatureAtParam :: (Additive v, Floating n) => Segment v n -> n -> n
-- curvatureAtParam s t = sqrt (curvatureSqAtParam s t)

-- curvatureSqAtParam :: (Additive v, Fractional n) => Segment v n -> n -> PosInf n
-- curvatureSqAtParam s t = case s of
--   Linear _ -> zero
--   Cubic {} -> (qs' * qs'' - (s' `dot` s'')^(2::Int)) / qs'^(3::Int)
--   where
--     s'   = s `tangentAtParam` t
--     s''  = s `secondDerivAtParam` t
--     qs'  = quadrance s'
--     qs'' = quadrance s''

-- Envelopes -----------------------------------------------------------

-- | Envelope specialised to cubic segments used for 'segmentEnvelope'.
--   This definition specialised to @V2 Double@ and @V3 Double@.
cubicEnvelope :: (Metric v, Floating n, Ord n) => v n -> v n -> v n -> v n -> Interval n
cubicEnvelope !c1 !c2 !c3 !v
  | l > 0     = I 0 u
  | u < 0     = I l 0
  | otherwise = I l u
  where
    I l u = foldr (\n (I x y) -> I (min x n) (max y n)) (singleton $ c3 `dot` v) (map f quadSol)
    f t   = (Cubic c1 c2 c3 `atParam` t) `dot` v
    quadSol = filter (\x -> (x>0) && (x<1)) $ quadForm a b c
    a = 3 * ((3 *^ c1 ^-^ 3 *^ c2 ^+^ c3) `dot` v)
    b = 6 * (((-2) *^ c1 ^+^ c2) `dot` v)
    c = (3 *^ c1) `dot` v
{-# SPECIALISE cubicEnvelope :: V2 Double -> V2 Double -> V2 Double -> V2 Double -> Interval Double #-}
{-# SPECIALISE cubicEnvelope :: V3 Double -> V3 Double -> V3 Double -> V3 Double -> Interval Double #-}

-- | Envelope of single segment without the 'Envelope' wrapper.
segmentEnvelope :: (Metric v, OrderedField n) => Segment v n -> Direction v n -> Interval n
segmentEnvelope !s = \(Dir v) -> case s of
  Linear l       -> let !x = l `dot` v
                    in  if x < 0 then I x 0 else I 0 x
  Cubic c1 c2 c3 -> cubicEnvelope c1 c2 c3 v
{-# INLINE segmentEnvelope #-}

instance (Metric v, OrderedField n) => Enveloped (Segment v n) where
  getEnvelope s = Envelope (segmentEnvelope s)
  {-# INLINE getEnvelope #-}

data Pair a b = Pair !a !b

getB :: Pair a b -> b
getB (Pair _ b) = b

-- | Calculate the envelope using a fold over segments.
envelopeOf
  :: (InSpace v n t, Metric v, OrderedField n)
  => Fold t (Segment v n)
  -> t
  -> v n
  -> Interval n
envelopeOf l = \ !t !w ->
  let f (Pair p e) !seg = Pair (p .+^ offset seg) e'
        where
          e' = combine e (moveBy (view _Point p `dot` w) $ segmentEnvelope seg (Dir w))
          --
          combine (I a1 b1) (I a2 b2) = I (min a1 a2) (max b1 b2)
          moveBy n (I a b)            = I (a + n) (b + n)
  in  getB $ foldlOf' l f (Pair origin (I 0 0)) t
{-# INLINE envelopeOf #-}

------------------------------------------------------------------------
-- 2D specific
------------------------------------------------------------------------

-- trace ---------------------------------------------------------------

-- | Calculate the trace using a fold over segments.
traceOf
  :: (InSpace V2 n t, OrderedField n)
  => Fold t (Segment V2 n)
  -> Point V2 n   -- trail start
  -> t            -- trail
  -> Point V2 n   -- trace start
  -> V2 n         -- trace direction
  -> Seq n        -- unsorted list of values
traceOf fold p0 trail p v@(V2 !vx !vy) = view _3 $ foldlOf' fold f (p0,False,mempty :: Seq n) trail
  where
    !theta = atan2A' vy vx
    !t2    = scaling (1/norm v)
      Sem.<> rotation (negated theta)
      Sem.<> translation (negated $ p^._Point)

    f (!q,!_nearStart,!ts) (Linear w)
      | parallel || not inRange = (q .+^ w, False, ts)
      | otherwise               = (q .+^ w, nearEnd, ts :> tv)
      where
        parallel = x1 == 0 && x2 /= 0
        nearEnd  = tw > 0.999
        inRange  = tw >= 0 && tw <= 1.001
        --
        tv = x3 / x1
        tw = x2 / x1
        --
        x1 =  v `crossZ` w
        x2 = pq `crossZ` v
        x3 = pq `crossZ` w
        pq  = q .-. p

    f (q,nearStart, ts) (Cubic c1 c2 c3) = (q .+^ c3, nearEnd, ts Sem.<> Seq.fromList ts')
      where
        P (V2 qx qy)  = papply t2 q
        c1'@(V2 _ y1) = apply t2 c1
        c2'@(V2 _ y2) = apply t2 c2
        c3'@(V2 _ y3) = apply t2 c3
        --
        a  =  3*y1 - 3*y2 + y3
        b  = -6*y1 + 3*y2
        c  =  3*y1
        d  =  qy

        tcs = filter (liftA2 (&&) (>= startLooking) (<= 1.0001)) (cubForm' 1e-8 a b c d)
        ts' = map ((+qx) . view _x . atParam (Cubic c1' c2' c3')) tcs

        -- if there was an intersecion near the end of the previous
        -- segment, don't look for an intersecion at the beggining of
        -- this one
        startLooking
          | nearStart = 0.0001
          | otherwise = 0

        -- if there's an intersection near the end of the segment, we
        -- don't look for an intersecion near the start of the next
        -- segment
        nearEnd = any (>0.9999) tcs
{-# INLINE traceOf #-}

-- crossings -----------------------------------------------------------

-- | The sum of /signed/ crossings of a path as we travel in the
--   positive x direction from a given point.
--
--     - A point is filled according to the 'Winding' fill rule, if the
--       number of 'Crossings' is non-zero (see 'isInsideWinding').
--
--     - A point is filled according to the 'EvenOdd' fill rule, if the
--       number of 'Crossings' is odd (see 'isInsideEvenOdd').
--
--   This is the 'HasQuery' result for 'Path's, 'Located' 'Trail's and
--   'Located' 'Loops'.
--
-- @
-- 'sample' :: 'Geometry.Path.Path' 'V2' 'Double'            -> 'Point' 'V2' 'Double' -> 'Crossings'
-- 'sample' :: 'Located' ('Geometry.Trail.Loop' 'V2' 'Double')  -> 'Point' 'V2' 'Double' -> 'Crossings'
-- 'sample' :: 'Located' ('Geometry.Trail.Trail' 'V2' 'Double') -> 'Point' 'V2' 'Double' -> 'Crossings'
-- @
--
--   Note that 'Line's have no inside or outside, so don't contribute
--   crossings.
newtype Crossings = Crossings Int
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

instance Sem.Semigroup Crossings where
  (<>) = coerce ((+) :: Int -> Int -> Int)
  {-# INLINE (<>) #-}

instance Monoid Crossings where
  mempty  = Crossings 0
  {-# INLINE mempty  #-}
  mappend = (Sem.<>)
  {-# INLINE mappend #-}

-- | Test whether the given point is inside the given path,
--   by testing whether the point's /winding number/ is nonzero. Note
--   that @False@ is /always/ returned for paths consisting of lines
--   (as opposed to loops), regardless of the winding number.
--
-- @
-- 'isInsideWinding' :: 'Geometry.Path.Path' 'V2' 'Double'            -> 'Point' 'V2' 'Double' -> 'Bool'
-- 'isInsideWinding' :: 'Located' ('Geometry.Trail.Loop' 'V2' 'Double')  -> 'Point' 'V2' 'Double' -> 'Bool'
-- 'isInsideWinding' :: 'Located' ('Geometry.Trail.Trail' 'V2' 'Double') -> 'Point' 'V2' 'Double' -> 'Bool'
-- @
isInsideWinding :: HasQuery t Crossings => t -> Point (V t) (N t) -> Bool
isInsideWinding t = (/= 0) . sample t
{-# INLINE isInsideWinding #-}

-- | Test whether the given point is inside the given path,
--   by testing whether a ray extending from the point in the positive
--   x direction crosses the path an even (outside) or odd (inside)
--   number of times.  Note that @False@ is /always/ returned for
--   paths consisting of lines (as opposed to loops), regardless of
--   the number of crossings.
--
-- @
-- 'isInsideEvenOdd' :: 'Geometry.Path.Path' 'V2' 'Double'            -> 'Point' 'V2' 'Double' -> 'Bool'
-- 'isInsideEvenOdd' :: 'Located' ('Geometry.Trail.Loop' 'V2' 'Double')  -> 'Point' 'V2' 'Double' -> 'Bool'
-- 'isInsideEvenOdd' :: 'Located' ('Geometry.Trail.Trail' 'V2' 'Double') -> 'Point' 'V2' 'Double' -> 'Bool'
-- @
isInsideEvenOdd :: HasQuery t Crossings => t -> Point (V t) (N t) -> Bool
isInsideEvenOdd t = odd . sample t
{-# INLINE isInsideEvenOdd #-}

-- | The crossings of a single segment given the query point and the
--   starting point of the segment.
segmentCrossings
  :: OrderedField n
  => Point V2 n -- ^ query point
  -> Point V2 n -- ^ start of segment
  -> Segment V2 n
  -> Crossings
segmentCrossings q a = \case
  Linear v       -> linearCrossings q a v
  Cubic c1 c2 c3 -> cubicCrossings q a c1 c2 c3
{-# INLINE segmentCrossings #-}

linearCrossings
  :: OrderedField n
  => Point V2 n -- ^ query point
  -> Point V2 n -- ^ start of segment
  -> V2 n -- ^ c1
  -> Crossings
linearCrossings q@(P (V2 _ qy)) a@(P (V2 _ ay)) v@(V2 _ vy)
  | ay <= qy && by > qy && isLeft     =  1
  | by <= qy && ay > qy && not isLeft = -1
  | otherwise                         =  0
  where
    isLeft = crossZ v (q .-. a) > 0
    by = ay + vy
{-# SPECIALISE linearCrossings :: Point V2 Double -> Point V2 Double -> V2 Double -> Crossings #-}

cubicCrossings
  :: OrderedField n
  => Point V2 n -- ^ query point
  -> Point V2 n -- ^ start of segment
  -> V2 n -- ^ c1
  -> V2 n -- ^ c2
  -> V2 n -- ^ c3
  -> Crossings
cubicCrossings (P (V2 qx qy)) (P (V2 ax ay)) c1@(V2 _ c1y) c2@(V2 _ c2y) c3@(V2 _ c3y)
  = sum $ map tTest ts'
    where
      -- potential t values at which the line y=qy intersects the segment
      ts' = filter inRange ts
      ts = cubForm ( 3*c1y - 3*c2y + c3y)
                   (-6*c1y + 3*c2y)
                   ( 3*c1y)
                   (ay - qy)
      inRange t = t >= 0 && t <= 1

      -- Check if the intersection point lies to the right of the query
      -- point. If it is return return the crossing sign depending on the
      -- tangent at that point.
      tTest t
        | ty == 0      = 0
        | ax + dx > qx = sign
        | otherwise    = 0
        where
        sign
          | theta > 0 =  1
          | otherwise = -1
        theta    = atan2A' ty tx ^. rad
        V2 dx _  = Cubic c1 c2 c3 `atParam` t
        V2 tx ty = Cubic c1 c2 c3 `tangentAtParam` t
{-# SPECIALISE cubicCrossings :: Point V2 Double -> Point V2 Double -> V2 Double -> V2 Double -> V2 Double -> Crossings #-}

-- | The parameters at which the segment is tangent to the given
--   direction.
paramsTangentTo
  :: OrderedField n
  => V2 n
  -> Segment V2 n
  -> [n]
paramsTangentTo (V2 tx ty) (Cubic (V2 x1 y1) (V2 x2 y2) (V2 x3 y3)) =
  filter (\x -> x >= 0 && x <= 1) (quadForm a b c)
    where
      a = tx*(y3 + 3*(y1 - y2)) - ty*(x3 + 3*(x1 - x2))
      b = 2*(tx*(y2 - 2*y1) - ty*(x2 - 2*x1))
      c = tx*y1 - ty*x1
paramsTangentTo _ (Linear {}) = []

-- | Split a 'Sectionable' up at the given parameters and return each
--   segment, in order.
splitAtParams :: (InSpace v n t, Ord n, Fractional n, Sectionable t) => t -> [n] -> [t]
splitAtParams t = unsafeSplitAtParams t . nub . sort . filter (\x -> x >= 0 && x < 1)

-- | Split a sectionable between a list of times. The list should be
--   sorted, distinct and between 0 and 1.
unsafeSplitAtParams :: (InSpace v n t, Fractional n, Sectionable t) => t -> [n] -> [t]
unsafeSplitAtParams seg0 ts0 = build $ \(|>) z ->
  let go !_ seg []     = seg |> z
      go t0 seg (t:ts) = s1 |> go t s2 ts
        where
          -- We want to split where t would lie on the original segment
          -- [0,1] and the partial segment @seg@ we've got to split
          -- corresponds to [t0,1] of the original segment. To get the
          -- new parameter we shift back to the origin (-t0) and rescale
          -- (* 1/(1-t0)).
          --
          -- seg0
          -- [-----|----------|-------------]
          -- 0     t0         t             1
          --
          -- seg
          --       [----------|-------------]
          --       t0         t             1
          --
          -- seg shifted
          -- [----------|-------------]
          -- 0        t - t0        1 - t0
          --
          -- seg scaled
          -- [---------------|--------------]
          -- 0      (t - t0) / (1 - t0)     1
          --
          t' = (t - t0) / (1 - t0)
          (s1,s2) = seg `splitAtParam` t'
  in go 0 seg0 ts0
  -- does fusion actually help here?
  -- would probably be better if ts was given as a (unboxed) vector

-- | Checks whether the points on the cubic segment lie in a straight
--   line.
-- collinear :: OrderedField n => n -> Segment V2 n -> Bool
-- collinear _   Linear {}        = True
-- collinear eps (Cubic c1 c2 c3) = undefined

-- | Check if two segments are approximately equal.
segmentsEqual
  :: OrderedField n
  => n
  -> Segment V2 n
  -> Segment V2 n
  -> Bool
segmentsEqual eps (Cubic a1 a2 a3) (Cubic b1 b2 b3)
  -- controlpoints equal within tol
  | distance a1 b1 < eps &&
    distance a2 b2 < eps &&
    distance a3 b3 < eps = True
  -- compare if both are collinear and close together
  -- - | dist < eps                  &&
  --   collinear ((eps-dist)/2) cb1 &&
  --   collinear ((eps-dist)/2) cb2 = True
  | otherwise = False
  -- where dist = max (abs $ ld b0) (abs $ ld b3)
  --       ld   = distance lineDistance (Line a0 a3)
segmentsEqual _ _ _ = undefined

------------------------------------------------------------------------
-- Closing segments
------------------------------------------------------------------------

-- | A ClosingSegment is used to determine how to close a loop. A linear
--   closing means to close a trail with a straight line. A cubic closing
--   segment means close the trail with a cubic bezier with control
--   points c1 and c2.
data ClosingSegment v n = LinearClosing | CubicClosing !(v n) !(v n)
  deriving Functor

type instance V (ClosingSegment v n) = v
type instance N (ClosingSegment v n) = n

instance (Eq1 v, Eq n) => Eq (ClosingSegment v n) where
  LinearClosing      == LinearClosing      = True
  CubicClosing b1 c1 == CubicClosing b2 c2 = eq1 b1 b2 && eq1 c1 c2
  _                  ==     _              = False
  {-# INLINE (==) #-}

instance Show1 v => Show1 (ClosingSegment v) where
  liftShowsPrec x y d = \case
    LinearClosing      -> showString "linearClosing"
    CubicClosing v1 v2 -> showParen (d > 10) $
      showString "cubicClosing " . liftShowsPrec x y 11 v1 . showChar ' '
                                 . liftShowsPrec x y 11 v2

instance (Show1 v, Show n) => Show (ClosingSegment v n) where
  showsPrec = showsPrec1

instance (Metric v, Foldable v, Num n) => Transformable (ClosingSegment v n) where
  transform t (CubicClosing c1 c2) = CubicClosing (apply t c1) (apply t c2)
  transform _ LinearClosing        = LinearClosing
  {-# INLINE transform #-}

instance NFData (v n) => NFData (ClosingSegment v n) where
  rnf = \case
    LinearClosing      -> ()
    CubicClosing c1 c2 -> rnf c1 `seq` rnf c2
  {-# INLINE rnf #-}

instance Hashable1 v => Hashable1 (ClosingSegment v) where
  liftHashWithSalt f s = \case
    LinearClosing      -> s0
    CubicClosing c1 c2 -> hws (hws s1 c1) c2
    where
      s0 = hashWithSalt s (0::Int)
      s1 = hashWithSalt s (1::Int)
      hws = liftHashWithSalt f
  {-# INLINE liftHashWithSalt #-}

instance (Hashable1 v, Hashable n) => Hashable (ClosingSegment v n) where
  hashWithSalt = hashWithSalt1
  {-# INLINE hashWithSalt #-}

instance Serial1 v => Serial1 (ClosingSegment v) where
  serializeWith f = \case
    LinearClosing      -> putWord8 0
    CubicClosing c1 c2 -> putWord8 1 >> fv c1 >> fv c2
    where fv = serializeWith f
  {-# INLINE serializeWith #-}

  deserializeWith m = getWord8 >>= \case
    0 -> return LinearClosing
    _ -> CubicClosing `liftM` mv `ap` mv
    where mv = deserializeWith m
  {-# INLINE deserializeWith #-}

instance (Serial1 v, Serial n) => Serial (ClosingSegment v n) where
  serialize = serializeWith serialize
  {-# INLINE serialize #-}
  deserialize = deserializeWith deserialize
  {-# INLINE deserialize #-}

instance (Serial1 v, Binary.Binary n) => Binary.Binary (ClosingSegment v n) where
  put = serializeWith Binary.put
  {-# INLINE put #-}
  get = deserializeWith Binary.get
  {-# INLINE get #-}

instance (Serial1 v, Cereal.Serialize n) => Cereal.Serialize (ClosingSegment v n) where
  put = serializeWith Cereal.put
  {-# INLINE put #-}
  get = deserializeWith Cereal.get
  {-# INLINE get #-}

-- | Create a linear closing segment.
linearClosing :: ClosingSegment v n
linearClosing = LinearClosing
{-# INLINE linearClosing #-}

-- | Create a closing segment with control points @c1@ and @c2@.
cubicClosing :: v n -> v n -> ClosingSegment v n
cubicClosing = CubicClosing
{-# INLINE cubicClosing #-}

-- | Return the segment that closes a trail given the offset from the
--   start of the trail to the start of the closing segment.
closingSegment :: (Functor v, Num n) => v n -> ClosingSegment v n -> Segment v n
closingSegment off LinearClosing        = Linear (negated off)
closingSegment off (CubicClosing c1 c2) = Cubic c1 c2 (negated off)
{-# INLINE closingSegment #-}

------------------------------------------------------------------------
-- Fixed segments
------------------------------------------------------------------------

-- | @FixedSegment@s are like 'Segment's except that they have
--   absolute locations.  @FixedSegment v@ is isomorphic to @Located
--   (Segment Closed v)@, as witnessed by 'mkFixedSeg' and
--   'fromFixedSeg', but @FixedSegment@ is convenient when one needs
--   the absolute locations of the vertices and control points.
data FixedSegment v n
  = FLinear !(Point v n) !(Point v n)
  | FCubic  !(Point v n) !(Point v n) !(Point v n) !(Point v n)
  deriving (Show, Read, Eq)

type instance V (FixedSegment v n) = v
type instance N (FixedSegment v n) = n

instance Each (FixedSegment v n) (FixedSegment v' n') (Point v n) (Point v' n') where
  each f (FLinear p0 p1)      = FLinear <$> f p0 <*> f p1
  each f (FCubic p0 p1 p2 p3) = FCubic  <$> f p0 <*> f p1 <*> f p2 <*> f p3
  {-# INLINE each #-}

-- | Reverses the control points.
instance Reversing (FixedSegment v n) where
  reversing (FLinear p0 p1)      = FLinear p1 p0
  reversing (FCubic p0 p1 p2 p3) = FCubic p3 p2 p1 p0
  {-# INLINE reversing #-}

instance (Additive v, Foldable v, Num n) => Transformable (FixedSegment v n) where
  transform t = over each (transform t)
  {-# INLINE transform #-}

instance (Additive v, Num n) => HasOrigin (FixedSegment v n) where
  moveOriginTo o = over each (moveOriginTo o)
  {-# INLINE moveOriginTo #-}

instance (Metric v, OrderedField n) => Enveloped (FixedSegment v n) where
  getEnvelope f = moveTo p (getEnvelope s)
    where (p, s) = viewLoc $ f ^. fixed
  {-# INLINE getEnvelope #-}

instance (Metric v, OrderedField n)
      => HasArcLength (FixedSegment v n) where
  arcLengthBounded m s = arcLengthBounded m (s ^. fixed)
  arcLengthToParam m s = arcLengthToParam m (s ^. fixed)

instance (Metric v, OrderedField n) => HasSegments (FixedSegment v n) where
  segments = fixed . located
  {-# INLINE segments #-}
  offset = offset . view fixed
  {-# INLINE offset #-}
  numSegments _ = 1
  {-# INLINE numSegments #-}

instance NFData (v n) => NFData (FixedSegment v n) where
  rnf = \case
    FLinear p1 p2      -> rnf p1 `seq` rnf p2
    FCubic p1 p2 p3 p4 -> rnf p1 `seq` rnf p2 `seq` rnf p3
                          `seq` rnf p4
  {-# INLINE rnf #-}

instance Hashable1 v => Hashable1 (FixedSegment v) where
  liftHashWithSalt f s = \case
    FLinear p1 p2      -> hws (hws s0 p1) p2
    FCubic p1 p2 p3 p4 -> hws (hws (hws (hws s1 p1) p2) p3) p4
    where
      s0 = hashWithSalt s (0::Int)
      s1 = hashWithSalt s (1::Int)
      hws s' = liftHashWithSalt f s' . view _Point
  {-# INLINE liftHashWithSalt #-}

instance (Hashable1 v, Hashable n) => Hashable (FixedSegment v n) where
  hashWithSalt = hashWithSalt1
  {-# INLINE hashWithSalt #-}

instance Serial1 v => Serial1 (FixedSegment v) where
  serializeWith f = \case
    FLinear p1 p2      -> putWord8 0 >> fv p1 >> fv p2
    FCubic p1 p2 p3 p4 -> putWord8 1 >> fv p1 >> fv p2 >> fv p3 >> fv p4
    where fv = serializeWith f
  {-# INLINE serializeWith #-}

  deserializeWith m = getWord8 >>= \case
    0 -> FLinear `liftM` mv `ap` mv
    _ -> FCubic `liftM` mv `ap` mv `ap` mv `ap` mv
    where mv = deserializeWith m
  {-# INLINE deserializeWith #-}

instance (Serial1 v, Serial n) => Serial (FixedSegment v n) where
  serialize = serializeWith serialize
  {-# INLINE serialize #-}
  deserialize = deserializeWith deserialize
  {-# INLINE deserialize #-}

instance (Serial1 v, Binary.Binary n) => Binary.Binary (FixedSegment v n) where
  put = serializeWith Binary.put
  {-# INLINE put #-}
  get = deserializeWith Binary.get
  {-# INLINE get #-}

instance (Serial1 v, Cereal.Serialize n) => Cereal.Serialize (FixedSegment v n) where
  put = serializeWith Cereal.put
  {-# INLINE put #-}
  get = deserializeWith Cereal.get
  {-# INLINE get #-}

-- | Fixed segments and located segments are isomorphic.
fixed :: (Additive v, Num n) => Iso' (FixedSegment v n) (Located (Segment v n))
fixed = iso fromFixedSeg mkFixedSeg
{-# INLINE fixed #-}

-- | Make a fixed segment from a located segment.
mkFixedSeg :: (Additive v, Num n) => Located (Segment v n) -> FixedSegment v n
mkFixedSeg = \case
  Loc p (Linear v)       -> FLinear p (p .+^ v)
  Loc p (Cubic c1 c2 x2) -> FCubic  p (p .+^ c1) (p .+^ c2) (p .+^ x2)
{-# INLINE mkFixedSeg #-}

-- | Make a located segment from a fixed one.
fromFixedSeg :: (Additive v, Num n) => FixedSegment v n -> Located (Segment v n)
fromFixedSeg = \case
  FLinear p1 p2      -> straight (p2 .-. p1) `at` p1
  FCubic x1 c1 c2 x2 -> bezier3 (c1 .-. x1) (c2 .-. x1) (x2 .-. x1) `at` x1
{-# INLINE fromFixedSeg #-}

type instance Codomain (FixedSegment v n) = Point v

instance (Additive v, Num n) => Parametric (FixedSegment v n) where
  atParam (FLinear p1 p2) t = lerp t p2 p1
  atParam (FCubic x1 c1 c2 x2) t = p3
    where p11 = lerp t c1 x1
          p12 = lerp t c2 c1
          p13 = lerp t x2 c2

          p21 = lerp t p12 p11
          p22 = lerp t p13 p12

          p3  = lerp t p22 p21
  {-# INLINE atParam #-}

instance Num n => DomainBounds (FixedSegment v n)

instance (Additive v, Num n) => EndValues (FixedSegment v n) where
  atStart (FLinear p0 _)     = p0
  atStart (FCubic  p0 _ _ _) = p0
  {-# INLINE atStart #-}
  atEnd   (FLinear _ p1)     = p1
  atEnd   (FCubic _ _ _ p1 ) = p1
  {-# INLINE atEnd #-}

instance (Additive v, Fractional n) => Sectionable (FixedSegment v n) where
  splitAtParam (FLinear p0 p1) t = (left, right)
    where left  = FLinear p0 p
          right = FLinear p  p1
          p = lerp t p1 p0
  splitAtParam (FCubic p0 c1 c2 p1) t = (left, right)
    where left  = FCubic p0 a b cut
          right = FCubic cut c d p1
          -- first round
          a   = lerp t c1 p0
          p   = lerp t c2 c1
          d   = lerp t p1 c2
          -- second round
          b   = lerp t p a
          c   = lerp t d p
          -- final round
          cut = lerp t c b
  {-# INLINE splitAtParam #-}

  reverseDomain (FLinear p0 p1)      = FLinear p1 p0
  reverseDomain (FCubic p0 p1 p2 p3) = FCubic p3 p2 p1 p0
  {-# INLINE reverseDomain #-}

-- closest :: FixedSegment v n -> Point v n -> [n]
-- closest = undefined

segmentParametersAtDirection
  :: OrderedField n
  => V2 n
  -> Segment V2 n
  -> [n]
segmentParametersAtDirection
  (V2 tx ty)
  (Cubic (V2 x1 y1) (V2 x2 y2) (V2 x3 y3)) =
  filter (\x -> x >= 0 && x <= 1) $ quadForm a b c
    where
      a = tx*(y3 + 3*(y1 - y2)) - ty*(x3 + 3*(x1 - x2))
      b = 2*(tx*(y2 - 2*y1) - ty*(x2 - 2*x1))
      c = tx*y1 - ty*x1
segmentParametersAtDirection _ _ = []
{-# INLINE segmentParametersAtDirection#-}

bezierParametersAtDirection
  :: OrderedField n
  => V2 n
  -> FixedSegment V2 n
  -> [n]
bezierParametersAtDirection
  (V2 tx ty)
  (FCubic (P (V2 x0 y0)) (P (V2 x1 y1)) (P (V2 x2 y2)) (P (V2 x3 y3))) =
  filter (\x -> x >= 0 && x <= 1) $ quadForm a b c
    where
      a = tx*((y3 - y0) + 3*(y1 - y2)) - ty*((x3 - x0) + 3*(x1 - x2))
      b = 2*(tx*((y2 + y0) - 2*y1) - ty*((x2 + x0) - 2*x1))
      c = tx*(y1 - y0) - ty*(x1 - x0)
bezierParametersAtDirection _ _ = []
{-# INLINE bezierParametersAtDirection#-}

