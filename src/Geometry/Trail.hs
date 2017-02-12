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
-- This module defines /trails/, translationally invariant paths through
-- space. Trails form a central part of the geometry API, so the
-- documentation for this module merits careful study.
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
    -- * Trails
    Line (..)
  , Loop (..)
  , loopClosingSegment
  , lineFromSegments
  , Trail (..)
  , wrapLine, wrapLoop
  , fixTrail

    -- * Prisms
  , _Line
  , _Loop
  , _LocLine
  , _LocLoop

    -- * Modification
  , closeLine
  , closeTrail
  , glueLine
  , glueTrail
  , reverseTrail
  , reverseLocTrail

    -- ** FromTrail
  , FromTrail (..)
  , fromSegments
  , fromVertices
  , fromLine
  , fromLoop
  , fromTrail
  , fromLocLine
  , fromLocLoop
  , fromOffsets

    -- ** Internal functions
  , lineEnv
  , loopEnv
  , trailEnv
  , lineSegParam
  ) where

import           Control.DeepSeq                    (NFData (..))
import           Control.Lens                       hiding (at, transform)
import           Control.Monad
import qualified Data.Binary                        as Binary
import           Data.Bytes.Get                     (getWord8)
import           Data.Bytes.Put                     (putWord8)
import           Data.Bytes.Serial
import           Data.Coerce
import qualified Data.Foldable                      as F
import           Data.Functor.Classes
import           Data.Functor.Contravariant         (phantom)
import           Data.Hashable
import           Data.Hashable.Lifted
import           Data.Semigroup
import           Data.Sequence                      (Seq)
import qualified Data.Sequence                      as Seq
import qualified Data.Serialize                     as Cereal
import           Data.Traversable                   (mapAccumL)
import           Numeric.Interval.NonEmpty.Internal

import           Linear.Affine
import           Linear.Metric
import           Linear.V2
import           Linear.V3
import           Linear.Vector


import           Geometry.Envelope
import           Geometry.Located
import           Geometry.Parametric
import           Geometry.Query
import           Geometry.Segment
import           Geometry.Space
import           Geometry.Trace
import           Geometry.Transform

------------------------------------------------------------------------
-- The line type
------------------------------------------------------------------------

type MetricSpace v n = (Metric v, OrderedField n)

-- | A line is a sequence of segments. Lines have no position.
data Line v n = Line !(Seq (Segment v n)) !(v n)

type instance V (Line v n) = v
type instance N (Line v n) = n
type instance Codomain (Line v n) = v

instance Show1 v => Show1 (Line v) where
  liftShowsPrec x y d line = showParen (d > 10) $
    showString "fromSegments " . liftShowList x y (toListOf segments line)

instance (Show1 v, Show n) => Show (Line v n) where
  showsPrec = showsPrec1

instance (Additive v, Num n) => Semigroup (Line v n) where
  Line s1 o1 <> Line s2 o2 = Line (s1 <> s2) (o1 ^+^ o2)
  {-# INLINE (<>) #-}

instance (Additive v, Num n) => Monoid (Line v n) where
  mappend = (<>)
  {-# INLINE mappend #-}
  mempty = Line mempty zero
  {-# INLINE mempty #-}

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

-- | Envelope of a line without a 'Envelope' wrapper. This is specialsed
--   to @V2 Double@ and @V3 Double@ and marked as @INLINEABLE@.
lineEnv :: MetricSpace v n => Line v n -> v n -> Interval n
lineEnv = envelopeOf segments
{-# SPECIALIZE lineEnv :: Line V2 Double -> V2 Double -> Interval Double #-}
{-# SPECIALIZE lineEnv :: Line V3 Double -> V3 Double -> Interval Double #-}
{-# INLINEABLE [0] lineEnv #-}

instance MetricSpace v n => Enveloped (Line v n) where
  getEnvelope l = Envelope (lineEnv l)
  {-# INLINE getEnvelope #-}

-- | Unsorted line trace, specialised to @Double@.
lineTrace :: OrderedField n => Line V2 n -> Point V2 n -> V2 n -> [n]
lineTrace l = traceOf segments origin l
{-# SPECIALIZE lineTrace :: Line V2 Double -> Point V2 Double -> V2 Double -> [Double] #-}
{-# INLINEABLE [0] lineTrace #-}

instance OrderedField n => Traced (Line V2 n) where
  getTrace l = Trace (\p v -> mkSortedList $ lineTrace l p v)
  {-# INLINE getTrace #-}

lineCrossings :: OrderedField n => Line V2 n -> Point V2 n -> Crossings
lineCrossings = crossingsOf segments origin
{-# SPECIALIZE lineCrossings :: Line V2 Double -> Point V2 Double -> Crossings #-}
{-# INLINEABLE [0] lineCrossings #-}

instance OrderedField n => HasQuery (Line V2 n) Crossings where
  getQuery = coerce (lineCrossings :: Line V2 n -> Point V2 n -> Crossings)
  {-# INLINE getQuery #-}

instance (Additive v, Num n, Foldable v) => Transformable (Line v n) where
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

instance Num n => DomainBounds (Line v n) where
  domainUpper (Line ss _) = fromIntegral (Seq.length ss)
  {-# INLINE domainUpper #-}

instance NFData (v n) => NFData (Line v n) where
  rnf (Line ss o) = rnf ss `seq` rnf o
  {-# INLINE rnf #-}

data SPInt = SP !Int !Int

-- hash something foldable with variable length where the data type is
-- completely defined by its elements
hashFoldableWith :: Foldable f => (Int -> a -> Int) -> Int -> f a -> Int
hashFoldableWith h salt arr = finalise (F.foldl' step (SP salt 0) arr)
  where
    finalise (SP s l) = hashWithSalt s l
    step (SP s l) x   = SP (h s x) (l + 1)

instance Hashable1 v => Hashable1 (Line v) where
  liftHashWithSalt f s (Line ss o) =
    hashFoldableWith (liftHashWithSalt f) s ss `hws` o
    where hws = liftHashWithSalt f
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

-- instance Num n => EndValues (Line v n) where
--   atStart _ = zero
--   {-# INLINE atStart #-}
--   atEnd     = offset
--   {-# INLINE atEnd #-}

lineSegParam :: (Additive v, OrderedField n) => Int -> n -> Line v n -> v n
lineSegParam n p (Line ss _) = ifoldl f zero ss where
  f i v s
    | i == n    = s `atParam` p
    | otherwise = offset s ^+^ v

-- instance Parametric (Line v n)

------------------------------------------------------------------------
-- The Loop type
------------------------------------------------------------------------

-- | Loops are lines with a closing segment.
data Loop v n = Loop !(Line v n) !(ClosingSegment v n)

type instance V (Loop v n) = v
type instance N (Loop v n) = n
type instance Codomain (Loop v n) = v

-- | The 'Segment' that closes the loop.
loopClosingSegment :: (Functor v, Num n) => Loop v n -> Segment v n
loopClosingSegment (Loop t c) = closingSegment (offset t) c

loopEnv :: MetricSpace v n => Loop v n -> v n -> Interval n
loopEnv (Loop t c) w = hull (lineEnv t w) i
  where
    i   = moveBy (v `dot` w) $ segmentEnvelope seg w
    v   = offset t
    seg = closingSegment v c
    --
    moveBy n (I a b) = I (a + n) (b + n)
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

-- loopEnv :: MetricSpace v n => Loop v n -> v n -> Interval n
-- loopEnv = \ !t !v -> envelopeOf segments t v
-- {-# SPECIALIZE loopEnv :: Loop V2 Double -> V2 Double -> Interval Double #-}
-- {-# SPECIALIZE loopEnv :: Loop V3 Double -> V3 Double -> Interval Double #-}
-- {-# INLINEABLE [0] loopEnv #-}

instance MetricSpace v n => Enveloped (Loop v n) where
  getEnvelope = Envelope . loopEnv
  {-# INLINE getEnvelope #-}

loopTrace :: OrderedField n => Loop V2 n -> Point V2 n -> V2 n -> [n]
loopTrace l = traceOf segments origin l
{-# SPECIALIZE loopTrace :: Loop V2 Double -> Point V2 Double -> V2 Double -> [Double] #-}
{-# INLINEABLE [0] loopTrace #-}

instance OrderedField n => Traced (Loop V2 n) where
  getTrace l = Trace (\p v -> mkSortedList $ loopTrace l p v)
  {-# INLINE getTrace #-}

loopCrossings :: OrderedField n => Loop V2 n -> Point V2 n -> Crossings
loopCrossings = crossingsOf segments origin
{-# SPECIALIZE loopCrossings :: Loop V2 Double -> Point V2 Double -> Crossings #-}
{-# INLINEABLE [0] loopCrossings #-}

instance OrderedField n => HasQuery (Loop V2 n) Crossings where
  getQuery l = Query (loopCrossings l)
  {-# INLINE getQuery #-}

instance (MetricSpace v n, Foldable v) => Transformable (Loop v n) where
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

------------------------------------------------------------------------
-- Trail type
------------------------------------------------------------------------

-- | A trail is either a line or a loop.
data Trail v n
  = OpenTrail !(Line v n)
  | ClosedTrail !(Loop v n)

type instance V (Trail v n) = v
type instance N (Trail v n) = n
type instance Codomain (Trail v n) = v

instance Show1 v => Show1 (Trail v) where
  liftShowsPrec x y d = \case
    OpenTrail l   -> liftShowsPrec x y d l
    ClosedTrail l -> liftShowsPrec x y d l

instance (Show1 v, Show n) => Show (Trail v n) where
  showsPrec = showsPrec1

-- | Convert a 'Line' into a 'Trail'.
wrapLine :: Line v n -> Trail v n
wrapLine = OpenTrail

-- | Convert a 'Loop' into a 'Trail'.
wrapLoop :: Loop v n -> Trail v n
wrapLoop = ClosedTrail

-- | Prism onto a 'Line'.
_Line :: Prism' (Trail v n) (Line v n)
_Line = prism' OpenTrail $ \case OpenTrail t -> Just t; _ -> Nothing

-- | Prism onto a 'Loop'.
_Loop :: Prism' (Trail v n) (Loop v n)
_Loop = prism' ClosedTrail $ \case ClosedTrail t -> Just t; _ -> Nothing

-- | Prism onto a 'Located' 'Line'.
_LocLine :: Prism' (Located (Trail v n)) (Located (Line v n))
_LocLine = prism' (mapLoc OpenTrail) $ located (preview _Line)

-- | Prism onto a 'Located' 'Loop'.
_LocLoop :: Prism' (Located (Trail v n)) (Located (Loop v n))
_LocLoop = prism' (mapLoc ClosedTrail) $ located (preview _Loop)

-- | A generic eliminator for 'Trail', taking functions specifying what to
--   do in the case of a line or a loop.
withTrail :: (Line v n -> r) -> (Loop v n -> r) -> Trail v n -> r
withTrail lineR loopR = \case
  OpenTrail line   -> lineR line
  ClosedTrail loop -> loopR loop

-- | Turn a loop into a line by \"cutting\" it at the common start/end
--   point, resulting in a line which just happens to start and end at
--   the same place.
--
--   @cutLoop@ is right inverse to 'glueLine', that is,
--
--   @
--   glueLine . cutLoop === id
--   @
cutLoop :: MetricSpace v n => Loop v n -> Line v n
cutLoop (Loop Empty _) = Empty
cutLoop (Loop line c)  = line |> closingSegment (offset line) c

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
--   <<diagrams/src_Diagrams_Trail_closeLineEx.svg#diagram=closeLineEx&width=500>>
--
--   > closeLineEx = pad 1.1 . centerXY . hcat' (with & sep .~ 1)
--   >   $ [almostClosed # strokeLine, almostClosed # closeLine # strokeLoop]
closeLine :: Line v n -> Loop v n
closeLine line = Loop line LinearClosing

-- | 'closeTrail' is a variant of 'closeLine' for 'Trail', which
--   performs 'closeLine' on lines and is the identity on loops.
closeTrail :: Trail v n -> Trail v n
closeTrail = withTrail (ClosedTrail . closeLine) ClosedTrail

-- | Make a line into a loop by \"gluing\" the endpoint to the
--   starting point.  In particular, the offset of the final segment
--   is modified so that it ends at the starting point of the entire
--   trail.  Typically, you would first construct a line which you
--   know happens to end where it starts, and then call 'glueLine' to
--   turn it into a loop.
--
--   <<diagrams/src_Diagrams_Trail_glueLineEx.svg#diagram=glueLineEx&width=500>>
--
--   > glueLineEx = pad 1.1 . hsep 1
--   >   $ [almostClosed # strokeLine, almostClosed # glueLine # strokeLoop]
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

-- | @glueTrail@ is a variant of 'glueLine' which works on 'Trail's.
--   It performs 'glueLine' on lines and is the identity on loops.
glueTrail :: (Additive v, Num n) => Trail v n -> Trail v n
glueTrail = withTrail (ClosedTrail . glueLine) ClosedTrail

trailEnv :: MetricSpace v n => Trail v n -> v n -> Interval n
trailEnv t v = case t of
  OpenTrail l   -> lineEnv l v
  ClosedTrail l -> loopEnv l v
{-# INLINE trailEnv #-}

instance (Metric v, OrderedField n) => Enveloped (Trail v n) where
  getEnvelope t = Envelope (trailEnv t)
  {-# INLINE getEnvelope #-}

instance OrderedField n => Traced (Trail V2 n) where
  getTrace (OpenTrail l) = getTrace l
  getTrace (ClosedTrail l) = getTrace l
  {-# INLINE getTrace #-}

instance OrderedField n => HasQuery (Trail V2 n) Crossings where
  getQuery (OpenTrail l) = getQuery l
  getQuery (ClosedTrail l) = getQuery l
  {-# INLINE getQuery #-}

instance (Additive v, Num n) => HasSegments (Trail v n) where
  segments f (OpenTrail t)   = phantom (segments f t)
  segments f (ClosedTrail t) = phantom (segments f t)
  {-# INLINE segments #-}
  offset (OpenTrail t) = offset t
  offset (ClosedTrail t) = offset t
  {-# INLINE offset #-}
  numSegments (OpenTrail t)   = numSegments t
  numSegments (ClosedTrail t) = numSegments t
  {-# INLINE numSegments #-}

instance (Additive v, Num n) => AsEmpty (Trail v n) where
  _Empty = nearly (OpenTrail Empty) (\case OpenTrail Empty -> True; _ -> False)
  {-# INLINE _Empty #-}

instance (Metric v, Foldable v, OrderedField n) => Transformable (Trail v n) where
  {-# SPECIALISE instance Transformable (Trail V2 Double) #-}
  transform t (OpenTrail l) = OpenTrail (transform t l)
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

reverseTrail :: (Metric v, OrderedField n) => Trail v n -> Trail v n
reverseTrail = withTrail (OpenTrail . reverseLine) (OpenTrail . reverseLoop)
  where
    reverseLine = lineFromSegments . reverse . toListOf segments
    reverseLoop (Loop line c) = undefined line c

reverseLocTrail :: (Metric v, OrderedField n) => Located (Trail v n) -> Located (Trail v n)
reverseLocTrail (Loc p t) = Loc (p .+^ offset t) (reverseTrail t)

------------------------------------------------------------------------
-- From trail
------------------------------------------------------------------------

class FromTrail t where
  -- | Make a @t@ from a trail.
  fromLocTrail :: Located (Trail (V t) (N t)) -> t

instance FromTrail t => FromTrail (Located t) where
  fromLocTrail t = fromLocTrail t `at` loc t
  {-# INLINE fromLocTrail #-}

instance (Additive v, Num n) => FromTrail [Point v n] where
  fromLocTrail (viewLoc -> (p0, t)) = scanl (.+^) p0 offsets where
    offsets = toListOf (segments . to offset) t
  {-# INLINE fromLocTrail #-}

instance FromTrail (Trail v n) where
  fromLocTrail = unLoc
  {-# INLINE fromLocTrail #-}

instance MetricSpace v n => FromTrail (Line v n) where
  fromLocTrail = withTrail id cutLoop . unLoc
  {-# INLINE fromLocTrail #-}

instance MetricSpace v n => FromTrail (Loop v n) where
  fromLocTrail = withTrail glueLine id . unLoc
  {-# INLINE fromLocTrail #-}

fromLine :: (InSpace v n t, FromTrail t) => Line v n -> t
fromLine = fromLocTrail . (`at` origin) . review _Line
{-# INLINE fromLine #-}

fromLocLine :: (InSpace v n t, FromTrail t) => Located (Line v n) -> t
fromLocLine = fromLocTrail . review _LocLine
{-# INLINE fromLocLine #-}

fromLoop :: (InSpace v n t, FromTrail t) => Loop v n -> t
fromLoop = fromLocTrail . (`at` origin) . review _Loop
{-# INLINE fromLoop #-}

fromLocLoop :: (InSpace v n t, FromTrail t) => Located (Loop v n) -> t
fromLocLoop = fromLocTrail . review _LocLoop
{-# INLINE fromLocLoop #-}

fromTrail :: (InSpace v n t, FromTrail t) => Trail v n -> t
fromTrail = fromLocTrail . (`at` origin)
{-# INLINE fromTrail #-}

fromVertices :: (InSpace v n t, MetricSpace v n, FromTrail t) => [Point v n] -> t
fromVertices []       = fromLocTrail $ OpenTrail Empty `at` origin
fromVertices (p0:pss) = fromLocTrail $ OpenTrail (fromOffsets (go p0 pss)) `at` p0 where
  go _ []      = []
  go p (p2:ps) = (p2 .-. p) : go p2 ps

fromOffsets :: (InSpace v n t, FromTrail t) => [v n] -> t
fromOffsets vs = fromLine (lineFromSegments $ map Linear vs)
{-# INLINE fromOffsets #-}


-- | Construct a trail-like thing from a list of segments, with the
--   origin as the location.
--
--   <<diagrams/src_Diagrams_FromTrail_fromSegmentsEx.svg#diagram=fromSegmentsEx&height=200>>
--
--   > fromSegmentsEx = fromSegments
--   >   [ straight (r2 (1,1))
--   >   , bézier3  (r2 (1,1)) unitX unit_Y
--   >   , straight unit_X
--   >   ]
--   >   # centerXY # pad 1.1
fromSegments :: (InSpace v n t, FromTrail t) => [Segment v n] -> t
fromSegments segs = fromLocTrail (OpenTrail (lineFromSegments segs) `at` origin)
{-# INLINE fromSegments #-}

-- XXX not efficient
locatedSegments
  :: (InSpace v n t, HasSegments t)
  => Point v n
  -> t
  -> [Located (Segment v n)]
locatedSegments p0 = snd . mapAccumL f p0 . toListOf segments
  where
    f p seg = (p .+^ offset seg, seg `at` p)

fixTrail
  :: (Metric v, OrderedField n)
  => Located (Trail v n) -> [FixedSegment v n]
fixTrail (Loc p t)  = map (review fixed) (locatedSegments p t)

-- -- -- | Construct a trail-like thing from a located list of segments.
-- -- fromLocSegments :: FromTrail t => Located [Segment Closed (V t) (N t)] -> t
-- -- fromLocSegments = fromLocTrail . mapLoc trailFromSegments

-- -- -- | Construct a trail-like thing of linear segments from a list
-- -- --   of offsets, with the origin as the location.
-- -- --
-- -- --   <<diagrams/src_Diagrams_FromTrail_fromOffsetsEx.svg#diagram=fromOffsetsEx&width=300>>
-- -- --
-- -- --   > fromOffsetsEx = fromOffsets
-- -- --   >   [ unitX
-- -- --   >   , unitX # rotateBy (1/6)
-- -- --   >   , unitX # rotateBy (-1/6)
-- -- --   >   , unitX
-- -- --   >   ]
-- -- --   >   # centerXY # pad 1.1
-- -- fromOffsets :: FromTrail t => [Vn t] -> t
-- -- fromOffsets = fromLocTrail . (`at` origin) . trailFromOffsets

-- -- -- | Construct a trail-like thing of linear segments from a located
-- -- --   list of offsets.
-- -- fromLocOffsets :: (V t ~ v, N t ~ n, V (v n) ~ v, N (v n) ~ n, FromTrail t) => Located [v n] -> t
-- -- fromLocOffsets = fromLocTrail . mapLoc trailFromOffsets

-- -- -- | Construct a trail-like thing connecting the given vertices with
-- -- --   linear segments, with the first vertex as the location.  If no
-- -- --   vertices are given, the empty trail is used with the origin as
-- -- --   the location.
-- -- --
-- -- --   <<diagrams/src_Diagrams_FromTrail_fromVerticesEx.svg#diagram=fromVerticesEx&width=300>>
-- -- --
-- -- --   > import Data.List (transpose)
-- -- --   >
-- -- --   > fromVerticesEx =
-- -- --   >   ( [ pentagon 1
-- -- --   >     , pentagon 1.3 # rotateBy (1/15)
-- -- --   >     , pentagon 1.5 # rotateBy (2/15)
-- -- --   >     ]
-- -- --   >     # transpose
-- -- --   >     # concat
-- -- --   >   )
-- -- --   >   # fromVertices
-- -- --   >   # closeTrail # strokeTrail
-- -- --   >   # centerXY # pad 1.1
-- -- fromVertices :: FromTrail t => [Point (V t) (N t)] -> t
-- -- fromVertices []       = fromLocTrail (emptyTrail `at` origin)
-- -- fromVertices ps@(p:_) = fromLocTrail (trailFromSegments (segmentsFromVertices ps) `at` p)

-- -- segmentsFromVertices :: (Additive v, Num n) => [Point v n] -> [Segment Closed v n]
-- -- segmentsFromVertices []         = []
-- -- segmentsFromVertices vvs@(_:vs) = map straight (zipWith (flip (.-.)) vvs vs)

-- -- -- | Create a linear trail between two given points.
-- -- --
-- -- --   <<diagrams/src_Diagrams_FromTrail_twiddleEx.svg#diagram=twiddleEx&width=300>>
-- -- --
-- -- --   > twiddleEx
-- -- --   >   = mconcat ((~~) <$> hexagon 1 <*> hexagon 1)
-- -- --   >   # centerXY # pad 1.1
-- -- (~~) :: (InSpace v n t, FromTrail t) => Point v n -> Point v n -> t
-- -- p1 ~~ p2 = fromVertices [p1, p2]

-- -- -- | Given a concretely located trail, \"explode\" it by turning each
-- -- --   segment into its own separate trail.  Useful for (say) applying a
-- -- --   different style to each segment.
-- -- --
-- -- --   <<diagrams/src_Diagrams_FromTrail_explodeTrailEx.svg#diagram=explodeTrailEx&width=300>>
-- -- --
-- -- --   > explodeTrailEx
-- -- --   >   = pentagon 1
-- -- --   >   # explodeTrail  -- generate a list of diagrams
-- -- --   >   # zipWith lc [orange, green, yellow, red, blue]
-- -- --   >   # mconcat # centerXY # pad 1.1
-- -- explodeTrail :: (InSpace v n t, FromTrail t) => Located (Trail v n) -> [t]
-- -- explodeTrail = map (mkTrail . view fixed) . fixTrail
-- --   where
-- --     mkTrail = fromLocTrail . mapLoc (trailFromSegments . (:[]))




