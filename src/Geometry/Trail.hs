{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Trail
-- Copyright   :  (c) 2013-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module defines /lines/, /loops/, and /trails/, which represent
-- translation-invariant paths through space, along with many related
-- functions for working with them.
--
-- Related modules include:
--
-- * 'Path's ("Geometry.Path") are collections of 'Located'
--   ("Geometry.Located") trails.
--
-- * Trails are composed of 'Segment's (see "Geometry.Segment"), though
--   most users should not need to work with segments directly.
--
-----------------------------------------------------------------------------

module Geometry.Trail
  (
    -- * Lines, loops, and trails
    -- $trails

    Line (..)
  , Loop (..)
  , Trail (..)

    -- ** Prisms
    -- $prisms

  , _Line
  , _Loop
  , _LocLine
  , _LocLoop
  , _Trail
  , _LocTrail

    -- * Conversion
    -- $convert

  , wrapLine
  , wrapLoop

  , closeLine
  , closeTrail
  , glueLine
  , glueTrail
  , cutLoop
  , cutTrail

  , fixTrail

    -- * Construction and destruction
    -- ** Primitive constructors
    -- $prim

  , lineFromSegments
  , loopFromSegments

    -- ** FromTrail
    -- $fromtrail

  , FromTrail (..)
  , fromSegments
  , fromVertices
  , (~~)
  , fromLine
  , fromLoop
  , fromTrail
  , fromLocLine
  , fromLocLoop
  , fromOffsets
  , fromLocSegments
  , fromLocOffsets

    -- ** Destruction
    -- $destruct

  , withTrail
  , withLine

  , loopClosingSegment

  , trailLocSegments

  , linePoints
  , loopPoints
  , trailPoints

  , lineVertices
  , lineVertices'
  , loopVertices
  , loopVertices'
  , trailVertices
  , trailVertices'

  , explodeTrail

    -- * Internal functions
    -- $internal

  , lineEnv
  , loopEnv
  , trailEnv
  , lineTrace
  , loopTrace
  , trailTrace
  , lineSegParam

  ) where

import           Control.DeepSeq                    (NFData (..))
import           Control.Lens                       hiding (at, transform)
import           Control.Monad
import qualified Data.Binary                        as Binary
import           Data.Bytes.Get                     (getWord8)
import           Data.Bytes.Put                     (putWord8)
import           Data.Bytes.Serial
import           Data.Foldable
import qualified Data.Foldable                      as F
import           Data.Functor.Classes
import           Data.Functor.Contravariant         (phantom)
import           Data.Hashable
import           Data.Hashable.Lifted
import           Data.Monoid.Action
import qualified Data.Semigroup as Sem
import           Data.Sequence                      (Seq)
import qualified Data.Sequence                      as Seq
import qualified Data.Serialize                     as Cereal
import           Data.Traversable                   (mapAccumL)
import           Geometry.Angle
import           Geometry.TwoD.Transform
import           Numeric.Interval.NonEmpty.Internal

import           Linear.Affine
import           Linear.Metric
import           Linear.V2
import           Linear.V3
import           Linear.Vector

import           Geometry.Direction
import           Geometry.Envelope
import           Geometry.Located
import           Geometry.Parametric
import           Geometry.Query
import           Geometry.Segment
import           Geometry.Space
import           Geometry.Trace
import           Geometry.Transform

-- $trails
--
-- A /trail/ is either a /line/ or a /loop/.
--
-- * A 'Line' (think "train line", not "straight line") is a path
--   through space that may end in a different place than where it
--   started. (A line may happen to end in the same place that it
--   starts but this would just be a coincidence.)  Lines are not
--   closed and hence they are never filled, even if they happen to
--   start and end at the same place.
--
-- * A 'Loop' is a closed path through space which ends in the same
--   place it starts, and hence can be filled (at least in 2D; in
--   higher dimensions filling a loop probably does not make sense).
--
-- * The 'Trail' type is simply the sum of 'Line' and 'Loop', i.e.
--   it is either one or the other.
--
-- Trails do not have an absolute location, i.e. they are
-- /translationally invariant/.  If you want a concretely positioned
-- trail, use the 'Located' wrapper.

-- $prim
-- These functions directly construct a line or loop from a list of
-- segments.  They can occasionally be useful if you want to directly
-- construct some segments (/e.g./ using a function like 'bezier3'),
-- but typically you would use one of the @fromXXX@ functions in the
-- next section instead.

------------------------------------------------------------------------
-- The line type
------------------------------------------------------------------------

-- | A 'Line' (think "train line", not "straight line") is a
--   translationally invariant path through space.  Lines are not
--   closed and hence they are never filled, even if they happen to
--   start and end at the same place.
--
--   To construct a 'Line', use 'fromVertices', 'fromOffsets', ('~~'),
--   'fromSegments', or any of the other @from...@ functions in this
--   module.  The 'Semigroup' (and 'Monoid') instance can be used to
--   construct bigger 'Line's out of smaller ones by concatenation.
--
--   To turn a 'Line' into a 'Loop', use 'closeLine' or 'glueLine'; to
--   turn a 'Line' into a 'Trail', use 'wrapLine'.  More generally,
--   use 'fromLine' to turn a 'Line' into any instance of 'FromTrail'.
--
--   To reverse a 'Line', use 'reversing'.
--
--   Lines are represented as a sequence of 'Segment's, concatenated
--   end to end, together with the cached offset vector from the start
--   of the line to the end.
data Line v n = Line !(Seq (Segment v n)) !(v n)

type instance V (Line v n) = v
type instance N (Line v n) = n
type instance Codomain (Line v n) = v

instance (Eq1 v, Eq n) => Eq (Line v n) where
  Line s1 _ == Line s2 _ = length s1 == length s1 && eq1 (toList s1) (toList s2)
  {-# INLINE (==) #-}

instance Show1 v => Show1 (Line v) where
  liftShowsPrec x y d line = showParen (d > 10) $
    showString "fromSegments " . liftShowList x y (toListOf segments line)
instance (Show1 v, Show n) => Show (Line v n) where
  showsPrec = showsPrec1

instance (Additive v, Num n) => Sem.Semigroup (Line v n) where
  Line s1 o1 <> Line s2 o2 = Line (s1 Sem.<> s2) (o1 ^+^ o2)
  {-# INLINE (<>) #-}

instance (Additive v, Num n) => Monoid (Line v n) where
  mappend = (Sem.<>)
  {-# INLINE mappend #-}
  mempty = Line mempty zero
  {-# INLINE mempty #-}

instance Floating n => Action (Angle n) (Line V2 n) where
  act = rotate

lineSeq :: Lens' (Line v n) (Seq (Segment v n))
lineSeq f (Line s o) = f s <&> \s' -> Line s' o
{-# INLINE lineSeq #-}

instance HasSegments (Line v n) where
  segments = lineSeq . folded
  {-# INLINE segments #-}
  offset (Line _ o) = o
  {-# INLINE offset #-}
  numSegments (Line ss _) = Seq.length ss
  {-# INLINE numSegments #-}

-- [folds-over-trails]
--
-- Many of the folds over trails involve keeping the current position
-- and some state. To make sure everything's strict we use a Pair.
-- However this is still not ideal because it involves boxing and
-- unboxing the position and state at every step, even with a
-- SPECIALISE.
--
-- Normally GHC is smart enough to turn strict accumulators into unboxed
-- tuples; however, this isn't possible in our case because the
-- definition of foldl' for Data.Seq isn't inlined. This is for the best
-- because the definition is quite large, leading to slow compile times and
-- masses of generated core for little performance benefit.
--
-- So for the important calculations (envelope, crossings) we make a
-- custom unpacked version of Pair for each specialisation and use RULES
-- to implement them. It's a bit more boilerplate but gives a 80-20%
-- speed improvement so the extra code is worth it.

data Pair a b = Pair !a !b

getB :: Pair a b -> b
getB (Pair _ b) = b

shift :: Num a => a -> Interval a -> Interval a
shift = \ n (I a b) -> I (a + n) (b + n)
{-# INLINE shift #-}

-- | Envelope of a line without an 'Envelope' wrapper. This is specialised
--   to @V2 Double@ and @V3 Double@.
lineEnv :: (Metric v, OrderedField n) => Line v n -> v n -> Interval n
lineEnv = \ !(Line t _) !w ->
  let f (Pair p e) !seg = Pair (p .+^ offset seg) e'
        where
          e' = hull e (shift (view _Point p `dot` w) $ segmentEnvelope seg (Dir w))
  in  getB $ foldl' f (Pair origin (I 0 0)) t
{-# SPECIALIZE lineEnv :: Line V3 Double -> V3 Double -> Interval Double #-}
{-# INLINE [0] lineEnv #-}

{-# RULES
  "lineEnv/Double" lineEnv = lineEnv2Double
  #-}

data LE2D = LE2D !Double !Double !Double !Double

-- GHC isn't smart enough to float out the segment unpacking (when using
-- offset/segmentEnvelope) so we do it ourselves
lineEnv2Double :: Line V2 Double -> V2 Double -> Interval Double
lineEnv2Double = \ !(Line t _) !w ->
  let f (LE2D x y a b) = \case
        Linear v@(V2 dx dy)       ->
          let !d = v `dot` w
              !s = V2 x y `dot` w
              !d' = d + s
          in  if d < 0
                then LE2D (x+dx) (y+dy) (min a d') b
                else LE2D (x+dx) (y+dy) a (max b d')
        Cubic c1 c2 c3@(V2 dx dy) ->
          case cubicEnvelope c1 c2 c3 w of
            I a' b' ->
              let !s = V2 x y `dot` w
              in  LE2D (x+dx) (y+dy) (min a (s+a')) (max b (s+b'))
  in  case foldl' f (LE2D 0 0 0 0) t of
        LE2D _ _ a b -> I a b

instance (Metric v, OrderedField n) => Enveloped (Line v n) where
  getEnvelope = \ l -> Envelope (\(Dir v) -> lineEnv l v)
  {-# INLINE getEnvelope #-}

-- | Unsorted line trace, specialised to @Double@.
lineTrace :: OrderedField n => Line V2 n -> Point V2 n -> V2 n -> Seq n
lineTrace = \l p v -> traceOf segments origin l p v
{-# SPECIALIZE lineTrace :: Line V2 Double -> Point V2 Double -> V2 Double -> Seq Double #-}

instance OrderedField n => Traced (Line V2 n) where
  getTrace = Trace . lineTrace
  {-# INLINE getTrace #-}

instance (Additive v, Foldable v, Num n) => Transformable (Line v n) where
  {-# SPECIALISE instance Transformable (Line V2 Double) #-}
  {-# SPECIALISE instance Transformable (Line V3 Double) #-}
  transform t (Line ss o) = Line (transform t ss) (apply t o)

-- | Construct a line from a list of segments.
lineFromSegments :: (Additive v, Num n) => [Segment v n] -> Line v n
lineFromSegments = F.foldr (<|) Empty
{-# INLINE lineFromSegments #-}

instance (Additive v, Num n) => AsEmpty (Line v n) where
  _Empty = nearly (Line mempty zero) (\(Line s _) -> Seq.null s)
  {-# INLINE _Empty #-}

instance (Additive v, Num n) => Cons (Line v n) (Line v n) (Segment v n) (Segment v n) where
  _Cons = prism consL unconsL where
    consL (s, Line ss o) = Line (s <| ss) (o ^+^ offset s)
    unconsL (Line ss o) = case ss of
      s :< ss' -> Right (s, Line ss' (o ^-^ offset s))
      _        -> Left mempty
  {-# INLINE _Cons #-}

instance (Additive v, Num n) => Snoc (Line v n) (Line v n) (Segment v n) (Segment v n) where
  _Snoc = prism snocL unsnocL where
    snocL (Line ss o, s) = Line (ss |> s) (o ^+^ offset s)
    unsnocL (Line ss o)  = case ss of
      ss' :> s -> Right (Line ss' (o ^-^ offset s), s)
      _        -> Left mempty
  {-# INLINE _Snoc #-}

instance NFData (v n) => NFData (Line v n) where
  rnf (Line ss o) = rnf ss `seq` rnf o
  {-# INLINE rnf #-}

instance Hashable1 v => Hashable1 (Line v) where
  liftHashWithSalt h s0 (Line ss _) =
    F.foldl' (\s a -> liftHashWithSalt h s a) s0 ss
      `hashWithSalt` Seq.length ss
  {-# INLINE liftHashWithSalt #-}

instance (Hashable1 v, Hashable n) => Hashable (Line v n) where
  hashWithSalt = hashWithSalt1
  {-# INLINE hashWithSalt #-}

instance Serial1 v => Serial1 (Line v) where
  serializeWith f (Line ss o) = do
    serializeWith (serializeWith f) ss
    serializeWith f o
  {-# INLINE serializeWith #-}

  deserializeWith m = do
    ss <- deserializeWith (deserializeWith m)
    o  <- deserializeWith m
    return (Line ss o)
  {-# INLINE deserializeWith #-}

instance (Serial1 v, Serial n) => Serial (Line v n) where
  serialize = serialize1
  {-# INLINE serialize #-}
  deserialize = deserialize1
  {-# INLINE deserialize #-}

instance (Serial1 v, Binary.Binary n) => Binary.Binary (Line v n) where
  put = serializeWith Binary.put
  {-# INLINE put #-}
  get = deserializeWith Binary.get
  {-# INLINE get #-}

instance (Serial1 v, Cereal.Serialize n) => Cereal.Serialize (Line v n) where
  put = serializeWith Cereal.put
  {-# INLINE put #-}
  get = deserializeWith Cereal.get
  {-# INLINE get #-}

-- | An eliminator for @Trail@ based on eliminating lines: if the
--   trail is a line, the given function is applied; if it is a loop, it
--   is first converted to a line with 'cutLoop'.  That is,
--
-- @
-- withLine f === 'withTrail' f (f . 'cutLoop')
-- @
withLine :: (Additive v, Num n) => (Line v n -> r) -> Trail v n -> r
withLine f = withTrail f (f . cutLoop)

lineSegParam :: (Additive v, OrderedField n) => Int -> n -> Line v n -> v n
lineSegParam n p (Line ss _) = ifoldl f zero ss where
  f i v s
    | i == n    = s `atParam` p
    | otherwise = offset s ^+^ v

instance (Additive v, Num n) => EndValues (Line v n) where
  atStart = const zero
  {-# INLINE atStart #-}
  atEnd (Line _ o) = o
  {-# INLINE atEnd #-}

instance (Additive v, Num n) => TangentEndValues (Line v n) where
  tangentAtStart = \case
    s :< _ -> tangentAtStart s
    _      -> zero
  {-# INLINE tangentAtStart #-}
  tangentAtEnd = \case
    _ :> s -> tangentAtEnd s
    _      -> zero
  {-# INLINE tangentAtEnd #-}

instance (Additive v, Num n) => Reversing (Line v n) where
  reversing = \(Line ss o) -> Line (reversing $ fmap reversing ss) (negated o)

------------------------------------------------------------------------
-- The Loop type
------------------------------------------------------------------------

-- | A 'Loop' is a translationally invariant, /closed/ path through
--   space.
--
--   To construct a 'Loop', use 'fromVertices', 'fromOffsets',
--   'fromSegments', or any of the other @from...@ functions in this
--   module, or make a 'Line' and then call 'closeLine' or 'glueLine'.
--
--   To turn a 'Loop' into a 'Line', use 'cutLoop'; to turn a 'Loop'
--   into a 'Trail', use 'wrapLoop'.
--
--   To reverse a 'Loop', use 'reversing'.
--
--   Loops are represented as a 'Line' together with a closing
--   segment.
data Loop v n = Loop !(Line v n) !(ClosingSegment v n)
  deriving Eq

type instance V (Loop v n) = v
type instance N (Loop v n) = n
type instance Codomain (Loop v n) = v

-- | Construct a @Loop@ from a list of segments and a 'ClosingSegment'.
loopFromSegments :: (Additive v, Num n) => [Segment v n] -> ClosingSegment v n -> Loop v n
loopFromSegments = Loop . lineFromSegments
{-# INLINE loopFromSegments #-}

-- | Get the 'Segment' that closes the loop.
loopClosingSegment :: (Functor v, Num n) => Loop v n -> Segment v n
loopClosingSegment (Loop t c) = closingSegment (offset t) c
{-# INLINE loopClosingSegment #-}

loopEnv :: (Metric v, OrderedField n) => Loop v n -> Direction v n -> Interval n
loopEnv (Loop t c) (Dir w) = hull (lineEnv t w) i
  where
    i   = shift (v `dot` w) $ segmentEnvelope seg (Dir w)
    v   = offset t
    seg = closingSegment v c
{-# INLINE loopEnv #-}

instance Show1 v => Show1 (Loop v) where
  liftShowsPrec x y d (Loop line closing) = showParen (d > 10) $
    showString "loopFromSegments " . liftShowList x y (toListOf segments line)
                                   . showChar ' '
                                   . liftShowsPrec x y 11 closing

instance (Show1 v, Show n) => Show (Loop v n) where
  showsPrec = showsPrec1

instance (Additive v, Num n) => HasSegments (Loop v n) where
  segments = \f (Loop t c) -> segments f t *> phantom (f (closingSegment (offset t) c))
  {-# INLINE segments #-}
  offset = const zero
  {-# INLINE offset #-}
  numSegments (Loop l _) = numSegments l + 1
  {-# INLINE numSegments #-}

instance (Metric v, OrderedField n) => Enveloped (Loop v n) where
  getEnvelope = Envelope . loopEnv
  {-# INLINE getEnvelope #-}

loopTrace :: OrderedField n => Loop V2 n -> Point V2 n -> V2 n -> Seq n
loopTrace = \l p v -> traceOf segments origin l p v
{-# SPECIALIZE loopTrace :: Loop V2 Double -> Point V2 Double -> V2 Double -> Seq Double #-}

instance OrderedField n => Traced (Loop V2 n) where
  getTrace l = Trace (\p v -> loopTrace l p v)
  {-# INLINE getTrace #-}

loopCrossings :: OrderedField n => Loop V2 n -> Point V2 n -> Crossings
loopCrossings = \(Loop (Line l o) cs) q ->
  let f (Pair a c) s = Pair (a .+^ offset s) (c + segmentCrossings q a s)
  in  case foldl' f (Pair origin 0) l of
        Pair a c -> c + segmentCrossings q a (closingSegment o cs)
{-# NOINLINE loopCrossings #-}

instance Floating n => Action (Angle n) (Loop V2 n) where
  act = rotate

data LCD = LCD !Crossings !Double !Double

mkP2 :: a -> a -> Point V2 a
mkP2 x y = P (V2 x y)

loopCrossingsDouble :: Loop V2 Double -> Point V2 Double -> Crossings
loopCrossingsDouble = \(Loop (Line l o) cs) q ->
  let f (LCD c x y) s = case s of
        Linear v@(V2 dx dy)       -> LCD (c + linearCrossings q (mkP2 x y) v) (x+dx) (y+dy)
        Cubic c1 c2 c3@(V2 dx dy) -> LCD (c + cubicCrossings q (mkP2 x y) c1 c2 c3) (x+dx) (y+dy)
  in  case foldl' f (LCD 0 0 0) l of
        LCD c x y -> c + segmentCrossings q (mkP2 x y) (closingSegment o cs)

{-# RULES
 "loopCrossings/Double" loopCrossings = loopCrossingsDouble
 #-}

instance OrderedField n => HasQuery (Loop V2 n) Crossings where
  getQuery l = Query (loopCrossings l)
  {-# INLINE getQuery #-}

instance (Metric v, Foldable v, Num n) => Transformable (Loop v n) where
  {-# SPECIALISE instance Transformable (Loop V2 Double) #-}
  transform t (Loop l c) = Loop (transform t l) (transform t c)

instance NFData (v n) => NFData (Loop v n) where
  rnf (Loop l c) = rnf l `seq` rnf c
  {-# INLINE rnf #-}

instance Hashable1 v => Hashable1 (Loop v) where
  liftHashWithSalt f s (Loop l c) =
    liftHashWithSalt f s l `hws` c
    where hws = liftHashWithSalt f
  {-# INLINE liftHashWithSalt #-}

instance (Hashable1 v, Hashable n) => Hashable (Loop v n) where
  hashWithSalt = hashWithSalt1
  {-# INLINE hashWithSalt #-}

instance Serial1 v => Serial1 (Loop v) where
  serializeWith f (Loop l c) = do
    serializeWith f l
    serializeWith f c
  {-# INLINE serializeWith #-}

  deserializeWith m = do
    l <- deserializeWith m
    c <- deserializeWith m
    return (Loop l c)
  {-# INLINE deserializeWith #-}

instance (Serial1 v, Serial n) => Serial (Loop v n) where
  serialize = serialize1
  {-# INLINE serialize #-}
  deserialize = deserialize1
  {-# INLINE deserialize #-}

instance (Serial1 v, Binary.Binary n) => Binary.Binary (Loop v n) where
  put = serializeWith Binary.put
  {-# INLINE put #-}
  get = deserializeWith Binary.get
  {-# INLINE get #-}

instance (Serial1 v, Cereal.Serialize n) => Cereal.Serialize (Loop v n) where
  put = serializeWith Cereal.put
  {-# INLINE put #-}
  get = deserializeWith Cereal.get
  {-# INLINE get #-}

instance (Additive v, Num n) => EndValues (Loop v n) where
  atStart = const zero
  atEnd = const zero


instance (Additive v, Num n) => Reversing (Loop v n) where
  reversing = glueLine . reversing . cutLoop

instance (Additive v, Num n) => TangentEndValues (Loop v n) where
  tangentAtStart = \case
    Loop (s :< _) _ -> tangentAtStart s
    _               -> zero
  {-# INLINE tangentAtStart #-}
  tangentAtEnd (Loop line c) = tangentAtEnd $ closingSegment (offset line) c
  {-# INLINE tangentAtEnd #-}

------------------------------------------------------------------------
-- Trail type
------------------------------------------------------------------------

-- | A trail is either a line or a loop.
data Trail v n
  = OpenTrail !(Line v n)
  | ClosedTrail !(Loop v n)
  deriving Eq

type instance V (Trail v n) = v
type instance N (Trail v n) = n
type instance Codomain (Trail v n) = v

instance Show1 v => Show1 (Trail v) where
  liftShowsPrec x y d = \case
    OpenTrail l   -> liftShowsPrec x y d l
    ClosedTrail l -> liftShowsPrec x y d l

instance (Show1 v, Show n) => Show (Trail v n) where
  showsPrec = showsPrec1

instance (Additive v, Num n) => Reversing (Trail v n) where
  reversing = withTrail (OpenTrail . reversing) (ClosedTrail . reversing)

instance (Additive v, Num n) => Sem.Semigroup (Trail v n) where
  Empty <> t2 = t2
  t1 <> Empty = t1
  t1 <> t2 = flip withLine t1 $ \l1 ->
             flip withLine t2 $ \l2 ->
             OpenTrail (l1 Sem.<> l2)
  {-# INLINE (<>) #-}

instance (Additive v, Num n) => Monoid (Trail v n) where
  mempty = Empty
  mappend = (Sem.<>)

instance Floating n => Action (Angle n) (Trail V2 n) where
  act = rotate

-- | Convert a 'Line' into a 'Trail'.
wrapLine :: Line v n -> Trail v n
wrapLine = OpenTrail
{-# INLINE wrapLine #-}

-- | Convert a 'Loop' into a 'Trail'.
wrapLoop :: Loop v n -> Trail v n
wrapLoop = ClosedTrail
{-# INLINE wrapLoop #-}

------------------------------------------------------------
-- Prisms
------------------------------------------------------------

-- $prisms
--
-- A collection of 'Prism'\s that can be used to construct and
-- deconstruct trails, lines, and loops.

-- | Trails are either lines or loops.
_Trail :: Iso' (Trail v n) (Either (Line v n) (Loop v n))
_Trail = iso (withTrail Left Right) (either OpenTrail ClosedTrail)
{-# INLINE _Trail #-}

-- | Located trails are either located lines or located loops
_LocTrail :: Iso' (Located (Trail v n)) (Either (Located (Line v n)) (Located (Loop v n)))
_LocTrail = iso fromLocTr toLocTrail
  where
    fromLocTr  (Loc p (OpenTrail t))   = Left (Loc p t)
    fromLocTr  (Loc p (ClosedTrail t)) = Right (Loc p t)
    toLocTrail (Left (Loc p t))  = Loc p (OpenTrail t)
    toLocTrail (Right (Loc p t)) = Loc p (ClosedTrail t)
{-# INLINE _LocTrail #-}

-- | Prism onto a 'Line'.
_Line :: Prism' (Trail v n) (Line v n)
_Line = prism' OpenTrail $ \case OpenTrail t -> Just t; _ -> Nothing
{-# INLINE _Line #-}

-- | Prism onto a 'Loop'.
_Loop :: Prism' (Trail v n) (Loop v n)
_Loop = prism' ClosedTrail $ \case ClosedTrail t -> Just t; _ -> Nothing
{-# INLINE _Loop #-}

-- | Prism onto a 'Located' 'Line'.
_LocLine :: Prism' (Located (Trail v n)) (Located (Line v n))
_LocLine = prism' (mapLoc OpenTrail) $ located (preview _Line)
{-# INLINE _LocLine #-}

-- | Prism onto a 'Located' 'Loop'.
_LocLoop :: Prism' (Located (Trail v n)) (Located (Loop v n))
_LocLoop = prism' (mapLoc ClosedTrail) $ located (preview _Loop)
{-# INLINE _LocLoop #-}

-- | A generic eliminator for 'Trail', taking functions specifying what to
--   do in the case of a line or a loop.
withTrail :: (Line v n -> r) -> (Loop v n -> r) -> Trail v n -> r
withTrail lineR loopR = \case
  OpenTrail line   -> lineR line
  ClosedTrail loop -> loopR loop
{-# INLINE withTrail #-}

------------------------------------------------------------
-- Conversion
------------------------------------------------------------

-- $convert
--
-- Turning a line into a loop can be done with 'glueLine' or
-- 'closeLine', depending on whether the line already starts and ends
-- in the same place or not.  Turning a loop into a line can be done
-- with 'cutLoop'.  For convenience, all three functions have variants
-- that operate on trails.
--
-- To turn a line or loop into a trail, use 'wrapLine' or 'wrapLoop'.

-- | Turn a loop into a line by \"cutting\" it at the common start/end
--   point, resulting in a line which just happens to start and end at
--   the same place.
--
--   @cutLoop@ is right inverse to 'glueLine', that is,
--
--   @
--   glueLine . cutLoop === id
--   @
cutLoop :: (Additive v, Num n) => Loop v n -> Line v n
cutLoop (Loop Empty _) = Empty
cutLoop (Loop line c)  = line |> closingSegment (offset line) c
{-# INLINE cutLoop #-}

-- | 'cutTrail' is a variant of 'cutLoop' for 'Trail', which performs
--   'cutLoop' on loops and is the identity on lines.
cutTrail :: (Additive v, Num n) => Trail v n -> Trail v n
cutTrail = withTrail OpenTrail (OpenTrail . cutLoop)
{-# INLINE cutTrail #-}

-- | Make a line into a loop by adding a new linear segment from the
--   line's end to its start.
--
--   @closeLine@ does not have any particularly nice theoretical
--   properties, but can be useful /e.g./ when you want to make a
--   closed polygon out of a list of points where the initial point is
--   not repeated at the end.  To use 'glueLine', one would first have
--   to duplicate the initial vertex, like
--
-- @
-- 'glueLine' . 'lineFromVertices' $ ps ++ [head ps]
-- @
--
--   Using @closeLine@, however, one can simply
--
-- @
-- closeLine . lineFromVertices $ ps
-- @
--
--   <<diagrams/src_Geometry_Trail_closeLineEx.svg#diagram=closeLineEx&width=500>>
--
--   > closeLineEx :: Diagram V2
--   > closeLineEx = pad 1.1 . centerXY . hsep 1
--   >   $ [almostClosed # stroke, almostClosed # closeLine # stroke]
--   >
--   > almostClosed :: Line V2 Double
--   > almostClosed = fromOffsets $ map r2 [(2, -1), (-3, -0.5), (-2, 1), (1, 0.5)]
closeLine :: Line v n -> Loop v n
closeLine line = Loop line LinearClosing
{-# INLINE closeLine #-}

-- | 'closeTrail' is a variant of 'closeLine' for 'Trail', which
--   performs 'closeLine' on lines and is the identity on loops.
closeTrail :: Trail v n -> Trail v n
closeTrail = withTrail (ClosedTrail . closeLine) ClosedTrail
{-# INLINE closeTrail #-}

-- | Make a line into a loop by \"gluing\" the endpoint to the
--   starting point.  In particular, the offset of the final segment
--   is modified so that it ends at the starting point of the entire
--   trail.  Typically, you would first construct a line which you
--   know happens to end where it starts, and then call 'glueLine' to
--   turn it into a loop.
--
--   <<diagrams/src_Geometry_Trail_glueLineEx.svg#diagram=glueLineEx&width=500>>
--
--   > glueLineEx = pad 1.1 . hsep 1
--   >   $ [almostClosed # stroke, almostClosed # glueLine # stroke]
--   >
--   > almostClosed :: Line V2 Double
--   > almostClosed = fromOffsets $ map r2 [(2, -1), (-3, -0.5), (-2, 1), (1, 0.5)]
--
--   @glueLine@ is left inverse to 'cutLoop', that is,
--
--   @
--   glueLine . cutLoop === id
--   @
glueLine :: (Additive v, Num n) => Line v n -> Loop v n
glueLine (t :> Linear _)      = Loop t LinearClosing
glueLine (t :> Cubic c1 c2 _) = Loop t (CubicClosing c1 c2)
glueLine _                    = Loop Empty LinearClosing
{-# INLINE glueLine #-}

-- | @glueTrail@ is a variant of 'glueLine' which works on 'Trail's.
--   It performs 'glueLine' on lines and is the identity on loops.
glueTrail :: (Additive v, Num n) => Trail v n -> Trail v n
glueTrail = withTrail (ClosedTrail . glueLine) ClosedTrail
{-# INLINE glueTrail #-}

trailEnv :: (Metric v, OrderedField n) => Trail v n -> v n -> Interval n
trailEnv t v = case t of
  OpenTrail l   -> lineEnv l v
  ClosedTrail l -> loopEnv l (Dir v)
{-# INLINE trailEnv #-}

instance (Metric v, OrderedField n) => Enveloped (Trail v n) where
  getEnvelope t = Envelope (\(Dir v) -> trailEnv t v)
  {-# INLINE getEnvelope #-}

trailTrace :: OrderedField n => Trail V2 n -> Point V2 n -> V2 n -> Seq n
trailTrace = \t p v -> case t of
  OpenTrail l   -> lineTrace l p v
  ClosedTrail l -> loopTrace l p v
{-# SPECIALISE trailTrace :: Trail V2 Double -> Point V2 Double -> V2
   Double -> Seq Double #-}

instance OrderedField n => Traced (Trail V2 n) where
  getTrace = \l -> Trace (\p v -> trailTrace l p v)
  {-# INLINE getTrace #-}

instance OrderedField n => HasQuery (Trail V2 n) Crossings where
  getQuery = \ case
    OpenTrail _   -> Query $ \ _ -> 0
    ClosedTrail l -> getQuery l
  {-# INLINE getQuery #-}

instance (Additive v, Num n) => HasSegments (Trail v n) where
  segments f (OpenTrail t)   = phantom (segments f t)
  segments f (ClosedTrail t) = phantom (segments f t)
  {-# INLINE segments #-}
  offset (OpenTrail t)   = offset t
  offset (ClosedTrail t) = offset t
  {-# INLINE offset #-}
  numSegments (OpenTrail t)   = numSegments t
  numSegments (ClosedTrail t) = numSegments t
  {-# INLINE numSegments #-}

instance (Additive v, Num n) => AsEmpty (Trail v n) where
  _Empty = nearly (OpenTrail Empty) (\case OpenTrail Empty -> True; _ -> False)
  {-# INLINE _Empty #-}

instance (Metric v, Foldable v, Num n) => Transformable (Trail v n) where
  {-# SPECIALISE instance Transformable (Trail V2 Double) #-}
  transform t (OpenTrail l)   = OpenTrail (transform t l)
  transform t (ClosedTrail l) = ClosedTrail (transform t l)

instance NFData (v n) => NFData (Trail v n) where
  rnf = \case
    OpenTrail l   -> rnf l
    ClosedTrail l -> rnf l
  {-# INLINE rnf #-}

instance Hashable1 v => Hashable1 (Trail v) where
  liftHashWithSalt f s = \case
    OpenTrail l   -> liftHashWithSalt f s1 l
    ClosedTrail l -> liftHashWithSalt f s2 l
    where
      s1 = hashWithSalt s (0::Int)
      s2 = hashWithSalt s (1::Int)
  {-# INLINE liftHashWithSalt #-}

instance (Hashable1 v, Hashable n) => Hashable (Trail v n) where
  hashWithSalt = hashWithSalt1
  {-# INLINE hashWithSalt #-}

instance Serial1 v => Serial1 (Trail v) where
  serializeWith f = \case
    OpenTrail l   -> putWord8 0 >> serializeWith f l
    ClosedTrail l -> putWord8 1 >> serializeWith f l
  {-# INLINE serializeWith #-}

  deserializeWith m = getWord8 >>= \case
    0 -> OpenTrail `liftM` deserializeWith m
    _ -> ClosedTrail `liftM` deserializeWith m
  {-# INLINE deserializeWith #-}

instance (Serial1 v, Serial n) => Serial (Trail v n) where
  serialize = serialize1
  {-# INLINE serialize #-}
  deserialize = deserialize1
  {-# INLINE deserialize #-}

instance (Serial1 v, Binary.Binary n) => Binary.Binary (Trail v n) where
  put = serializeWith Binary.put
  {-# INLINE put #-}
  get = deserializeWith Binary.get
  {-# INLINE get #-}

instance (Serial1 v, Cereal.Serialize n) => Cereal.Serialize (Trail v n) where
  put = serializeWith Cereal.put
  {-# INLINE put #-}
  get = deserializeWith Cereal.get
  {-# INLINE get #-}

instance (Additive v, Num n) => EndValues (Trail v n) where
  atStart = \case
    OpenTrail l   -> atStart l
    ClosedTrail l -> atStart l
  atEnd = \case
    OpenTrail l   -> atEnd l
    ClosedTrail l -> atEnd l

------------------------------------------------------------------------
-- FromTrail
------------------------------------------------------------------------

-- $fromtrail
--
-- The functions in this section are all polymorphic in their output,
-- returning any instance of the 'FromTrail' class.  This means they
-- can be used to construct lines, loops, trails, or their 'Located'
-- variants, in addition to vertex lists, paths, and diagrams.
-- Functions like 'fromLine', 'fromLocLoop', /etc./ can also be used
-- to convert among various trail-like things.
--
-- There are many functions in other modules follow a similar pattern,
-- such as functions to construct various shapes like rectangles,
-- regular polygons, and so on; such functions can also be used to
-- construct any trail-like thing.

-- | @FromTrail@ instances are things which can be constructed from a
--   located trail. Instances include lines, loops, trails, lists of
--   vertices, paths, diagrams, and 'Located' variants of all the
--   above.
class FromTrail t where
  -- | Make a @t@ from a trail.
  fromLocTrail :: Located (Trail (V t) (N t)) -> t

instance FromTrail t => FromTrail (Located t) where
  fromLocTrail t = fromLocTrail t `at` loc t
  {-# INLINE fromLocTrail #-}

instance (Additive v, Num n) => FromTrail [Point v n] where
  fromLocTrail = trailPoints
  {-# INLINE fromLocTrail #-}

instance FromTrail (Trail v n) where
  fromLocTrail = unLoc
  {-# INLINE fromLocTrail #-}

instance (Metric v, OrderedField n) => FromTrail (Line v n) where
  fromLocTrail = withTrail id cutLoop . unLoc
  {-# INLINE fromLocTrail #-}

instance (Metric v, OrderedField n) => FromTrail (Loop v n) where
  fromLocTrail = withTrail glueLine id . unLoc
  {-# INLINE fromLocTrail #-}

-- | Construct a trail-like thing from a line, with the origin as the
--   location.
fromLine :: (InSpace v n t, FromTrail t) => Line v n -> t
fromLine = fromLocTrail . (`at` origin) . review _Line
{-# INLINE fromLine #-}

-- | Construct a trail-like thing from a 'Located' line.
fromLocLine :: (InSpace v n t, FromTrail t) => Located (Line v n) -> t
fromLocLine = fromLocTrail . review _LocLine
{-# INLINE fromLocLine #-}

-- | Construct a trail-like thing from a loop, placing it at the origin.
fromLoop :: (InSpace v n t, FromTrail t) => Loop v n -> t
fromLoop = fromLocTrail . (`at` origin) . review _Loop
{-# INLINE fromLoop #-}

-- | Construct a trail-like thing from a 'Located' loop.
fromLocLoop :: (InSpace v n t, FromTrail t) => Located (Loop v n) -> t
fromLocLoop = fromLocTrail . review _LocLoop
{-# INLINE fromLocLoop #-}

-- | Constuct a trail-like thing from a 'Trail', placing it at the origin.
fromTrail :: (InSpace v n t, FromTrail t) => Trail v n -> t
fromTrail = fromLocTrail . (`at` origin)
{-# INLINE fromTrail #-}


-- | Construct a trail-like thing from a list of segments, with the
--   origin as the location.
--
--   <<diagrams/src_Geometry_Trail_fromSegmentsEx.svg#diagram=fromSegmentsEx&height=200>>
--
--   > fromSegmentsEx = fromSegments
--   >   [ straight (r2 (1,1))
--   >   , bezier3  (r2 (1,1)) unitX unit_Y
--   >   , straight unit_X
--   >   ]
--   >   # centerXY # pad 1.1
fromSegments :: (InSpace v n t, FromTrail t) => [Segment v n] -> t
fromSegments segs = fromLocTrail (OpenTrail (lineFromSegments segs) `at` origin)
{-# INLINE fromSegments #-}

-- | Create a linear trail between two given points.
--
--   <<diagrams/src_Geometry_Trail_twiddleEx.svg#diagram=twiddleEx&width=300>>
--
--   > twiddleEx
--   >   = mconcat ((~~) <$> hexagon 1 <*> hexagon 1)
--   >   # centerXY # pad 1.1
(~~) :: (InSpace v n t, Metric v, OrderedField n, FromTrail t) => Point v n -> Point v n -> t
a ~~ b = fromVertices [a,b]

-- XXX not efficient
-- | Given a starting location for the first segment and an object
--   composed of segments, return a list of its 'Located' segments.
locatedSegments
  :: (InSpace v n t, HasSegments t)
  => Point v n
  -> t
  -> [Located (Segment v n)]
locatedSegments p0 = snd . mapAccumL f p0 . toListOf segments
  where
    f p seg = (p .+^ offset seg, seg `at` p)

-- | Convert a located trail into a list of segments with absolute
--   coordinates.  This may be particularly useful for backend
--   implementors
fixTrail
  :: (Metric v, OrderedField n)
  => Located (Trail v n) -> [FixedSegment v n]
fixTrail (Loc p t)  = map (review fixed) (locatedSegments p t)

-- | Construct a trail-like thing from a located list of segments.
fromLocSegments :: (InSpace v n t, FromTrail t) => Located [Segment v n] -> t
fromLocSegments = fromLocLine . mapLoc lineFromSegments

-- | Construct a trail-like thing of linear segments from a list
--   of offsets, with the origin as the location.
--
--   <<diagrams/src_Geometry_Trail_fromOffsetsEx.svg#diagram=fromOffsetsEx&width=300>>
--
--   > fromOffsetsEx = fromOffsets
--   >   [ unitX
--   >   , unitX # rotateBy (1/6)
--   >   , unitX # rotateBy (-1/6)
--   >   , unitX
--   >   ]
--   >   # centerXY # pad 1.1
fromOffsets :: (InSpace v n t, FromTrail t) => [v n] -> t
fromOffsets vs = fromLine (lineFromSegments $ map Linear vs)
{-# INLINE fromOffsets #-}

-- | Construct a trail-like thing of linear segments from a located
--   list of offsets.  Like 'fromOffsets', but with a starting
--   location specified.
fromLocOffsets :: (InSpace v n t, InSpace v n (v n), Metric v, OrderedField n, FromTrail t) => Located [v n] -> t
fromLocOffsets = fromLocLine . mapLoc fromOffsets

-- | Construct a trail-like thing connecting the given vertices with
--   linear segments, with the first vertex as the location.  If no
--   vertices are given, the empty trail is used with the origin as
--   the location.
--
--   <<diagrams/src_Geometry_Trail_fromVerticesEx.svg#diagram=fromVerticesEx&width=300>>
--
--   > import Data.List (transpose)
--   >
--   > fromVerticesEx =
--   >   ( [ pentagon 1
--   >     , pentagon 1.3 # rotateBy (1/15)
--   >     , pentagon 1.5 # rotateBy (2/15)
--   >     ]
--   >     # transpose
--   >     # concat
--   >   )
--   >   # fromVertices
--   >   # closeTrail # stroke
--   >   # centerXY # pad 1.1
fromVertices :: (InSpace v n t, Metric v, OrderedField n, FromTrail t) => [Point v n] -> t
fromVertices []         = fromLocTrail $ OpenTrail Empty `at` origin
fromVertices pps@(p:ps) = fromLocTrail $ OpenTrail (fromOffsets offsets) `at` p
  where offsets = zipWith (flip (.-.)) pps ps

-- | Given a concretely located trail, \"explode\" it by turning each
--   segment into its own separate trail.  Useful for (say) applying a
--   different style to each segment.
--
--   <<diagrams/src_Geometry_Trail_explodeTrailEx.svg#diagram=explodeTrailEx&width=300>>
--
--   > explodeTrailEx
--   >   = pentagon 1
--   >   # explodeTrail  -- generate a list of diagrams
--   >   # zipWith lc [orange, green, yellow, red, blue]
--   >   # mconcat # centerXY # pad 1.1
explodeTrail :: (InSpace v n t, Metric v, OrderedField n, FromTrail t) => Located (Trail v n) -> [t]
explodeTrail = map (mkTrail . view fixed) . fixTrail
  where
    mkTrail = fromLocTrail . mapLoc (fromSegments . (:[]))

-- | Convert a concretely located trail into a list of located segments.
trailLocSegments
  :: (Metric v, OrderedField n)
  => Located (Trail v n) -> [Located (Segment v n)]
trailLocSegments t = zipWith at (toListOf segments (unLoc t)) (trailPoints t)

------------------------------------------------------------------------

-- $destruct
-- Various ways to take lines, loops, or trails and extract
-- information from them.

-- Points --------------------------------------------------------------

-- | Extract the points of a concretely located trail, /i.e./ the points
--   where one segment ends and the next begins. Note that for loops,
--   the starting point will /not/ be repeated at the end.  If you
--   want this behavior, you can use 'cutTrail' to make the loop into
--   a line first, which happens to repeat the same point at the start
--   and end, /e.g./ with @trailPoints . mapLoc cutTrail@.
--
--   Note that it does not make sense to ask for the points of a
--   'Trail' by itself; if you want the points of a trail
--   with the first point at, say, the origin, you can use
--   @trailPoints . (\`at\` origin)@.
--
--   This function allows you "observe" the fact that trails are
--   implemented as lists of segments, which may be problematic if we
--   want to think of trails as parametric vector functions. This also
--   means that the behavior of this function may not be stable under
--   future changes to the implementation of trails.  For an
--   unproblematic version which only yields vertices at which there
--   is a sharp corner, excluding points where the trail is
--   differentiable, see 'trailVertices'.
--
--   This function is not re-exported from "Geometry"; to use
--   it, import "Geometry.Trail".
trailPoints
  :: (Additive v, Num n)
  => Located (Trail v n) -> [Point v n]
trailPoints (viewLoc -> (p,t))
  = withTrail (linePoints . (`at` p)) (loopPoints . (`at` p)) t

-- | Extract the segment join points of a concretely located line.  See
--   'trailPoints' for more information.
--
--   This function allows you "observe" the fact that lines are
--   implemented as lists of segments, which may be problematic if we
--   want to think of lines as parametric vector functions. This also
--   means that the behavior of this function may not be stable under
--   future changes to the implementation of trails.  For an
--   unproblematic version which only yields vertices at which there
--   is a sharp corner, excluding points where the trail is
--   differentiable, see 'lineVertices'.
--
--   This function is not re-exported from "Geometry"; to use
--   it, import "Geometry.Trail".
linePoints
  :: (Additive v, Num n)
  => Located (Line v n) -> [Point v n]
linePoints (viewLoc -> (p,t))
  = segmentPoints p . toListOf segments $ t

-- | Extract the segments comprising a loop: a list of closed
--   segments, and one final open segment.
loopSegments :: Loop v n -> ([Segment v n], ClosingSegment v n)
loopSegments (Loop t c) = (toListOf segments t, c)

-- | Extract the segment join points of a concretely located loop.  Note that the
--   initial vertex is not repeated at the end.  See 'trailPoints' for
--   more information.
--
--   This function allows you "observe" the fact that lines are
--   implemented as lists of segments, which may be problematic if we
--   want to think of lines as parametric vector functions. This also
--   means that the behavior of this function may not be stable under
--   future changes to the implementation of trails.  For an
--   unproblematic version which only yields vertices at which there
--   is a sharp corner, excluding points where the trail is
--   differentiable, see 'lineVertices'.
--
--   This function is not re-exported from "Geometry"; to use
--   it, import "Geometry.Trail".
loopPoints
  :: (Additive v, Num n)
  => Located (Loop v n) -> [Point v n]
loopPoints (viewLoc -> (p,t))
  = segmentPoints p . fst . loopSegments $ t

-- | Internal function to turn a list of segments into a list of vertices.
segmentPoints :: (Additive v, Num n) => Point v n -> [Segment v n] -> [Point v n]
segmentPoints p = scanl (.+^) p . map offset

-- Verticies -----------------------------------------------------------

tolerance :: OrderedField a => a
tolerance = 10e-16

-- | Extract the vertices of a concretely located trail.  Here a /vertex/
--   is defined as a non-differentiable point on the trail, /i.e./ a
--   sharp corner.  (Vertices are thus a subset of the places where
--   segments join; if you want all joins between segments, see
--   'trailPoints'.)  The tolerance determines how close the tangents
--   of two segments must be at their endpoints to consider the
--   transition point to be differentiable.
--
--   Note that for loops, the starting vertex will /not/ be repeated
--   at the end.  If you want this behavior, you can use 'cutTrail' to
--   make the loop into a line first, which happens to repeat the same
--   vertex at the start and end, /e.g./ with @trailVertices . mapLoc
--   cutTrail@.
--
--   It does not make sense to ask for the vertices of a 'Trail' by
--   itself; if you want the vertices of a trail with the first vertex
--   at, say, the origin, you can use @trailVertices . (\`at\`
--   origin)@.
trailVertices'
  :: (Metric v, OrderedField n)
  => n ->  Located (Trail v n) -> [Point v n]
trailVertices' toler (viewLoc -> (p,t))
  = withTrail (lineVertices' toler . (`at` p)) (loopVertices' toler . (`at` p)) t

-- | Like 'trailVertices'', with a default tolerance.
trailVertices
  :: (Metric v, OrderedField n)
  => Located (Trail v n) -> [Point v n]
trailVertices = trailVertices' tolerance

-- | Extract the vertices of a concretely located line.  See
--   'trailVertices' for more information.
lineVertices'
  :: (Metric v, OrderedField n)
  => n -> Located (Line v n) -> [Point v n]
lineVertices' toler (viewLoc -> (p,t))
  = segmentVertices' toler p . toListOf segments $ t

-- | Like 'lineVertices'', with a default tolerance.
lineVertices
  :: (Metric v, OrderedField n)
  => Located (Line v n) -> [Point v n]
lineVertices = lineVertices' tolerance

-- | Extract the vertices of a concretely located loop.  Note that the
--   initial vertex is not repeated at the end.  See 'trailVertices' for
--   more information.
loopVertices'
  :: (Metric v, OrderedField n)
  => n -> Located (Loop v n) -> [Point v n]
loopVertices' toler (viewLoc -> (p,t))
  | length segs > 1 = if far > toler then init ps else init . drop 1 $ ps
  | otherwise       = ps
  where
    far = quadrance ((signorm . tangentAtStart . head $ segs) ^-^
                       (signorm . tangentAtEnd   . last $ segs))
    segs = toListOf segments . cutLoop $ t
    ps = segmentVertices' toler p segs

-- | Same as 'loopVertices'', with a default tolerance.
loopVertices
  :: (Metric v, OrderedField n)
  => Located (Loop v n) -> [Point v n]
loopVertices = loopVertices' tolerance

-- | The vertices of a list of segments laid end to end.
--   The start and end points are always included in the list of
--   vertices.  The other points connecting segments are included if
--   the slope at the end of a segment is not equal to the slope at
--   the beginning of the next.  The 'toler' parameter is used to
--   control how close the slopes need to be in order to declare them
--   equal.
segmentVertices'
  :: (Metric v, OrderedField n)
  => n -> Point v n -> [Segment v n] -> [Point v n]
segmentVertices' toler p ts  =
  case ps of
    (x:_:_) -> x : select (drop 1 ps) ds ++ [last ps]
    _       -> ps
    where
      ds = zipWith far tans (drop 1 tans)
      tans = [(signorm . tangentAtStart $ s
              ,signorm . tangentAtEnd   $ s) | s <- ts]
      ps = scanl (.+^) p . map offset $ ts
      far p2 q2 = quadrance (snd p2 ^-^ fst q2) > toler

select :: [a] -> [Bool] -> [a]
select xs bs = map fst $ filter snd (zip xs bs)

