{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ViewPatterns               #-}
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
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE PatternSynonyms       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Trail
-- Copyright   :  (c) 2013-2016 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module defines /trails/, translationally invariant paths
-- through space.  Trails form a central part of the diagrams-lib API,
-- so the documentation for this module merits careful study.
--
-- Related modules include:
--
-- * The 'FromTrail' class ("Diagrams.FromTrail") exposes a generic
--   API for building a wide range of things out of trails.
--
-- * 'Path's ("Diagrams.Path") are collections of 'Located'
--   ("Diagrams.Located") trails.
--
-- * Trails are composed of 'Segment's (see "Diagrams.Segment"),
--   though most users should not need to work with segments directly.
--
-----------------------------------------------------------------------------

module Geometry.Trail where
  -- (
  --   -- * Trails
  --   Trail (..)
  -- , Trail' (..)
  -- , Line, Loop
  -- , Unboxable
  --   -- * ClosedSegent
  -- , Segment (..)
  -- , segmentEnvelope
  -- , vectorSegments
  -- , trailEnvelope

  --   -- * Vector internals
  --   -- ** Segment vectors
  -- , Line(..)

  --   -- ** Folds
  -- , foldSegments
  -- , foldSegmentsT
  -- , unboxEnvelope
  -- , U.Vector(V_Cubic)
  -- , U.MVector(MV_Cubic)
  -- ) where

import           Control.Lens                hiding (at, transform)
import           Control.Lens.Internal       (noEffect)
import qualified Data.Foldable               as F
import           Data.Semigroup
-- import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U
-- import qualified Data.Vector.Unboxed.Base    as U
import           Data.Word
import Data.Functor.Classes

-- import           Geometry.Angle
import           Geometry.Envelope
import           Geometry.Located
import           Geometry.Parametric
-- import           Geometry.Query
-- import           Geometry.Segment
import           Geometry.Space
-- import           Geometry.Trace
-- import           Geometry.Trail              (Line, Loop, SegTree (..),
--                                               Trail' (..), withTrail)
import           Geometry.Transform
import           Geometry.Segment
-- import           Geometry.TwoD.Path          (Crossings (..))
-- import           Geometry.TwoD.Transform
import Control.Monad.ST

import Numeric.Interval.NonEmpty.Internal

import Data.Functor.Contravariant (phantom)

-- import Segment2

import           Linear.Affine
import           Linear.Metric
import           Linear.V2
-- import           Linear.V3
import           Linear.Vector

-- import           Control.Applicative         (liftA2)

-- import           Geometry.FromTrail
-- import           Geometry.TwoD.Vector
import Data.Bits

import Control.Monad.Primitive

type Unboxable v n = (Metric v, HasLinearMap v, OrderedField n, U.Unbox (v n))

------------------------------------------------------------------------
-- Mutable lines
------------------------------------------------------------------------

-- Currently unexposed.
data MLine s v n = MLine !(U.MVector s Word32) !(U.MVector s (v n))
-- The first vector contains information about the length of the
-- line, the type of each segment and the offset of each segment.
--
--  In the first vector
--   - in 0th index: the total length of the line
--   - in the ith vector - the indexed in the second vector of the start
--     of the vector. If the 31st bit of the word is not set, it's a
--     'Linear' segment, if the 31st bit is set, it's a 'Cubic' segment.
--
-- In the second vector
--   - 0th index: total offset of the line
--   - offsets of each type of segment

-- so a square would look like:
--   Line [4,2,3,4] [0,0,1,0,0,1,-1,0,0,-1]
--
-- advantages
--    - segment offsets are in a packed vector (making transforms easy)
--    - O(1) indexing
--    - log(n) adding segments (since we double the vector if there's not enough space)
--
-- disadvantages:
--   - changing the type segment at location i is O(n-i) (but it doesn't
--     make much sense to do this for lines)
--   - maximum of 715827882 cubic segments (should be enough)
--   - slightly complicated internal way to represent offsets

-- transformLine
--   :: (PrimMonad m, Unboxable v n)
--   => Transformation v n -> MLine (PrimState m) v n -> m ()
-- transformLine t (MLine _ vs) =
--   F.for_ [0..M.length vs-1] $ \i -> do
--     v <- M.unsafeRead vs i
--     M.unsafeWrite vs i (apply t v)
-- {-# INLINE transformLine #-}

emptyMLine
  :: (PrimMonad m, Unboxable v n)
  => m (MLine (PrimState m) v n)
emptyMLine = do
  ws <- M.new 1
  vs <- M.new 1
  M.unsafeWrite ws 0 0
  M.unsafeWrite vs 0 zero
  return (MLine ws vs)
{-# INLINE emptyMLine #-}

-- | Add a segment to a line, growing the underlying arrays in the
--   line if nessessary.
addSegment
  :: (PrimMonad m, Unboxable v n)
  => Segment v n -> MLine (PrimState m) v n -> m (MLine (PrimState m) v n)
addSegment seg (MLine ws vs) = do
  n <- fromIntegral <$> M.unsafeRead ws 0
  i <- if n == 0
         then return 1
         else fromIntegral <$> M.unsafeRead ws (n+1)
  ws' <- unsafeEnsure ws (n+2)
  case seg of
    Linear v -> do
      M.unsafeWrite ws' (n+2) (fromIntegral $ i+1)
      vs' <- unsafeEnsure vs i
      M.unsafeWrite vs' i v
      o <- M.unsafeRead vs' 0
      M.unsafeWrite vs' 0 (o ^+^ v)
      return (MLine ws' vs')

    Cubic v1 v2 v3 -> do
      M.unsafeWrite ws' (n+2) (fromIntegral $ i+3)
      vs' <- unsafeEnsure vs (i+2)
      M.unsafeWrite vs'  i    v1
      M.unsafeWrite vs' (i+1) v2
      M.unsafeWrite vs' (i+2) v3
      o <- M.unsafeRead vs' 0
      M.unsafeWrite vs' 0 (o ^+^ v3)
      return (MLine ws' vs')
{-# INLINE addSegment#-}

-- | Ensure vector v has index i, doubling the size of the vector if
--   necessary.
unsafeEnsure
  :: (PrimMonad m, M.MVector v a)
  => v (PrimState m) a -> Int -> m (v (PrimState m) a)
unsafeEnsure v n
  | n < M.length v = return v
  | otherwise      = do
      let by = M.length v
      v' <- M.unsafeGrow v by
      M.basicInitialize $ M.basicUnsafeSlice (M.length v) by v'
      return v'
{-# INLINE unsafeEnsure #-}

unsafeFreezeLine :: (PrimMonad m, Unboxable v n) => MLine (PrimState m) v n -> m (Line v n)
unsafeFreezeLine (MLine mws mvs) = do
  ws <- U.unsafeFreeze mws
  vs <- U.unsafeFreeze mvs
  return $ Line ws vs
{-# INLINE unsafeFreezeLine #-}

-- | Construct a line out of segments.
mkLine :: Unboxable v n => [Segment v n] -> Line v n
mkLine segs = runST $ do
  l0 <- emptyMLine
  mLine <- F.foldrM addSegment l0 segs
  unsafeFreezeLine mLine
{-# INLINE mkLine #-}

-- | Construct a loop out of a line and a closing segment.
mkLoop :: Line v n -> ClosingSegment v n -> Loop v n
mkLoop = Loop
{-# INLINE mkLoop #-}

------------------------------------------------------------------------
-- The line type
------------------------------------------------------------------------

-- | A line is a sequence of segments. Lines have no position.
data Line v n = Line !(U.Vector Word32) !(U.Vector (v n))

type instance V (Line v n) = v
type instance N (Line v n) = n
type instance Codomain (Line v n) = v

lineSegments
  :: Unboxable v n
  => Fold (Line v n) (Segment v n)
lineSegments f (Line ws vs) = go 0 where
  !n = U.length ws
  go !i
    | i == n    = noEffect
    | otherwise = case testBit o 31 of
        False -> let !v  = U.unsafeIndex vs (clearBit o 31)
                 in  f (Linear v) *> go (i+1)
        True  -> let !v1 = U.unsafeIndex vs o
                     !v2 = U.unsafeIndex vs (o+1)
                     !v3 = U.unsafeIndex vs (o+2)
                 in  f (Cubic v1 v2 v3) *> go (i+1)
    where !o = fromIntegral (U.unsafeIndex ws i)
{-# INLINE lineSegments #-}

joinLine :: Unboxable v n => Line v n -> Line v n -> Line v n
joinLine (Line ws0 vs0) l2 = runST $ do
  ws <- U.thaw ws0
  vs <- U.thaw vs0
  mLine <- foldrMOf lineSegments addSegment (MLine ws vs) l2
  unsafeFreezeLine mLine

instance Unboxable v n => Semigroup (Line v n) where
  (<>) = joinLine
  {-# INLINE (<>) #-}
instance Unboxable v n => Monoid (Line v n) where
  mappend = joinLine
  {-# INLINE mappend #-}
  mempty = emptyLine
  {-# INLINE mempty #-}

instance Unboxable v n => HasSegments (Line v n) where
  segments = lineSegments
  {-# INLINE segments #-}
  offset (Line _ vs) = U.unsafeIndex vs 0
  {-# INLINE offset #-}

lineEnv :: Unboxable v n => Line v n -> v n -> Interval n
lineEnv !t !v = envelopeOf lineSegments t v
{-# SPECIALIZE lineEnv :: Line V2 Double -> V2 Double -> Interval Double #-}

instance Unboxable v n => Enveloped (Line v n) where
  {-# SPECIALISE instance Enveloped (Line V2 Double) #-}
  getEnvelope = Envelope . lineEnv

instance Unboxable v n => Transformable (Line v n) where
  {-# SPECIALISE instance Transformable (Line V2 Double) #-}
  transform t (Line ws vs) = Line ws (U.map (apply t) vs)

instance (Show1 v, Show n, Unboxable v n) => Show (Line v n) where
  showsPrec d line = showParen (d > 10) $
    showString "fromSegments " . liftShowList showsPrec showList (toListOf segments line)

instance Unboxable v n => AsEmpty (Line v n) where
  _Empty = nearly emptyLine isEmpty where
    isEmpty (Line ws _) = U.length ws == 1
  {-# INLINE _Empty #-}

lineSegs :: Unboxable v n => Iso' (Line v n) [Segment v n]
lineSegs = iso (toListOf lineSegments) mkLine

instance Unboxable v n => Cons (Line v n) (Line v n) (Segment v n) (Segment v n) where
  _Cons = lineSegs . _Cons . bimapping id (from lineSegs)
  {-# INLINE _Cons #-}

instance Unboxable v n => Snoc (Line v n) (Line v n) (Segment v n) (Segment v n) where
  _Snoc = lineSegs . _Snoc . bimapping (from lineSegs) id
  {-# INLINE _Snoc #-}

------------------------------------------------------------------------
-- The Loop type
------------------------------------------------------------------------

-- | Loops are lines with a closing segment.
data Loop v n = Loop !(Line v n) !(ClosingSegment v n)

type instance V (Loop v n) = v
type instance N (Loop v n) = n
type instance Codomain (Loop v n) = v

loopClosingSegment :: Unboxable v n => Loop v n -> Segment v n
loopClosingSegment (Loop t c) = closingSegment (offset t) c

loopEnv :: Unboxable v n => Loop v n -> v n -> Interval n
loopEnv (Loop t c) w = hull (lineEnv t w) i
  where
    i   = moveBy (v `dot` w) $ segmentEnvelope seg w
    v   = offset t
    seg = closingSegment v c
    --
    moveBy n (I a b) = I (a + n) (b + n)
{-# INLINE loopEnv #-}

instance (Show1 v, Show n, Unboxable v n) => Show (Loop v n) where
  showsPrec d (Loop line _) = showParen (d > 10) $
    showString "fromSegments " . liftShowList showsPrec showList (toListOf segments line) -- XXX BOGUS

instance Unboxable v n => Enveloped (Loop v n) where
  {-# SPECIALISE instance Enveloped (Loop V2 Double) #-}
  getEnvelope t = Envelope $ loopEnv t

instance Unboxable v n => HasSegments (Loop v n) where
  segments = \f (Loop t c) -> segments f t *> phantom (f (closingSegment (offset t) c))
  {-# INLINE segments #-}
  offset = const zero

instance Unboxable v n => Transformable (Loop v n) where
  {-# SPECIALISE instance Transformable (Loop V2 Double) #-}
  transform t (Loop l c) = Loop (transform t l) (transform t c)

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

withTrail :: (Line v n -> r) -> (Loop v n -> r) -> Trail v n -> r
withTrail lineR loopR = \case
  OpenTrail line   -> lineR line
  ClosedTrail loop -> loopR loop

emptyTrail :: Unboxable v n => Trail v n
emptyTrail = OpenTrail emptyLine

emptyLine :: Unboxable v n => Line v n
emptyLine = runST $ unsafeFreezeLine =<< emptyMLine

-- | Turn a loop into a line by \"cutting\" it at the common start/end
--   point, resulting in a line which just happens to start and end at
--   the same place.
--
--   @cutLoop@ is right inverse to 'glueLine', that is,
--
--   @
--   glueLine . cutLoop === id
--   @
cutLoop :: Unboxable v n => Loop v n -> Line v n
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

closeTrail :: Trail v n -> Trail v n
closeTrail = withTrail (ClosedTrail . closeLine) ClosedTrail

glueLine :: Unboxable v n => Line v n -> Loop v n
glueLine (t :> Linear _)      = Loop t LinearClosing
glueLine (t :> Cubic c1 c2 _) = Loop t (CubicClosing c1 c2)
glueLine _                    = Loop Empty LinearClosing

glueTrail :: Unboxable v n => Trail v n -> Trail v n
glueTrail = withTrail (ClosedTrail . glueLine) ClosedTrail

fromVertices :: (InSpace v n t, Unboxable v n, FromTrail t) => [Point v n] -> t
fromVertices []       = fromTrail $ OpenTrail Empty `at` origin
fromVertices (p0:pss) = fromTrail $ OpenTrail (fromOffsets (go p0 pss)) `at` p0 where
  go _ []      = []
  go p (p2:ps) = (p2 .-. p) : go p2 ps

fromOffsets :: (InSpace v n t, Unboxable v n, FromTrail t) => [v n] -> t -- Line v n
fromOffsets vs = fromTrail $ OpenTrail (mkLine $ map Linear vs) `at` origin

instance (Show1 v, Show n, Unboxable v n) => Show (Trail v n) where
  showsPrec d trail = showParen (d > 10) $
    showString "fromSegments " . liftShowList showsPrec showList (toListOf segments trail) -- XXX BOGUS

instance Unboxable v n => Enveloped (Trail v n) where
  getEnvelope (OpenTrail t)   = getEnvelope t
  getEnvelope (ClosedTrail t) = getEnvelope t
  {-# INLINE getEnvelope #-}

instance Unboxable v n => HasSegments (Trail v n) where
  segments f (OpenTrail t)   = phantom (segments f t)
  segments f (ClosedTrail t) = phantom (segments f t)
  {-# INLINE segments #-}
  offset (OpenTrail t) = offset t
  offset (ClosedTrail t) = offset t
  {-# INLINE offset #-}

instance Unboxable v n => Transformable (Trail v n) where
  {-# SPECIALISE instance Transformable (Trail V2 Double) #-}
  transform t (OpenTrail l) = OpenTrail (transform t l)
  transform t (ClosedTrail l) = ClosedTrail (transform t l)

reverseTrail :: Unboxable v n => Trail v n -> Trail v n
reverseTrail = withTrail (OpenTrail . reverseLine) (OpenTrail . reverseLoop)
  where
    reverseLine = mkLine . reverse . toListOf segments
    reverseLoop (Loop line c) = undefined

reverseLocTrail :: Unboxable v n => Located (Trail v n) -> Located (Trail v n)
reverseLocTrail (Loc p t) = Loc (p .+^ offset t) (reverseTrail t)


-- instance Unboxed (v n) => HasSegments (Line v n) where
--   segments = \f (Line v) -> phantom (vectorSegments origin f v)
--   {-# INLINE segments #-}

-- instance (HasLinearMap v, OrderedField n, U.Unbox (v n)) => Enveloped (Line v n) where
--   getEnvelope s = Envelope $ envelopeOf segments s
--   {-# INLINE getEnvelope #-}

-- instance Unboxable v n => Transformable (Line v n) where
--   transform = transformLine
--   {-# INLINE transform #-}

-- transformLine :: Unboxable v n => Transformation v n -> Line v n -> Line v n
-- transformLine t (Line s) = Line (U.map (transform t) s)
-- {-# INLINEABLE [0] transformLine #-}
-- {-# SPECIALISE transformLine :: Transformation V2 Double -> Line V2 Double -> Line V2 Double #-}
-- {-# SPECIALISE transformLine :: Transformation V3 Double -> Line V3 Double -> Line V3 Double #-}

-- instance Unboxable V2 n => Traced (Line V2 n) where
--   getTrace s = Trace $ traceOf segments s

-- type instance Codomain (Line v n) = v

------------------------------------------------------------------------
-- Trail' types
------------------------------------------------------------------------

-- -- | A Trail' can be either a line or a loop. Lines have their end
-- --   vector cached
-- data Trail' c v n where
--   ULine :: !(Line v n) -> !(v n)             -> Trail' Line v n
--   ULoop :: !(Line v n) -> !(OpenSegment v n) -> Trail' Loop v n

-- type Unboxable v n = (Metric v, HasLinearMap v, OrderedField n, U.Unbox (v n))

-- instance Unboxable v n => HasSegments (Trail' c v n) where
--   segments f (ULine v _) = phantom (segments f v)
--   segments f (ULoop (Line v) s) = phantom (vectorLoopSegments origin s f v)
--   {-# INLINE segments #-}

-- type instance V (Trail' c v n) = v
-- type instance N (Trail' c v n) = n

-- instance Unboxable v n => Transformable (Trail' c v n) where
--   transform t (ULine s v) = ULine (transform t s) (apply t v)
--   transform t (ULoop s o) = ULoop (transform t s) (transform t o)
--   {-# INLINE transform #-}

-- trailEnvelope :: Unboxable v n => Trail' c v n -> v n -> n
-- trailEnvelope = envelopeOf segments
-- -- {-# NOINLINE trailEnvelope #-}
-- {-# INLINEABLE [1] trailEnvelope #-}

-- {-# SPECIALISE trailEnvelope :: Trail' Loop V2 Double -> V2 Double -> Double #-}
-- {-# SPECIALISE trailEnvelope :: Trail' Line V2 Double -> V2 Double -> Double #-}
-- {-# SPECIALISE trailEnvelope :: Trail' c V3 Double -> V3 Double -> Double #-}

-- instance Unboxable v n => Enveloped (Trail' c v n) where
--   getEnvelope !t @ ULine {} = Envelope $ trailEnvelope t
--   getEnvelope !t @ ULoop {} = Envelope $ trailEnvelope t
--   -- getEnvelope (ULine s _)             = getEnvelope s
--   -- getEnvelope (ULoop (Line s) o) = Envelope $ \v ->
--   --   let (!(P p), !n) = unboxEnvelope' s v
--   --       nOff     = case o of
--   --         OpenLinear      -> p `dot` v
--   --         OpenCubic c1 c2 -> segmentEnvelope (Cubic (p ^-^ c1) (p ^-^ c2) p) v
--   --   in  max n nOff
--   {-# INLINE getEnvelope #-}

-- data Trail v n where
--   Trail :: !(Trail' c v n) -> Trail v n

-- type instance V (Trail v n) = v
-- type instance N (Trail v n) = n

-- instance Unboxable v n => Transformable (Trail v n) where
--   transform t (Trail s) = Trail (transform t s)
--   {-# INLINE transform #-}

-- instance Unboxable v n => Enveloped (Trail v n) where
--   getEnvelope (Trail s) = getEnvelope s
--   {-# INLINE getEnvelope #-}

-- instance Unboxable v n => FromTrail (Trail v n) where
--   {-# SPECIALISE instance FromTrail (Trail V2 Double) #-}
--   {-# SPECIALISE instance FromTrail (Trail V3 Double) #-}
--   fromTrail (Loc _ t) = Trail (fromT t)
--     where
--       fromT = withTrail (\(Line st)   -> ULine (fromSeg st))
--                         (\(Loop st o) -> ULoop (fromSeg st) o)
--       fromSeg = Line . U.fromList . map t2t . F.toList . op SegTree

--       t2t :: ClosedSegment v n -> Segment v n
--       t2t (Linear (OffsetClosed v))       = Linear v
--       t2t (Cubic c1 c2 (OffsetClosed c3)) = Cubic c1 c2 c3

------------------------------------------------------------------------
-- 2D
------------------------------------------------------------------------

-- foldTrail
--   :: (Additive v, Num n, Semigroup m, U.Unbox (v n))
--   => (Point v n -> v n -> m) -- ^ line to
--   -> (Point v n -> v n -> v n -> v n -> m) -- ^ curve to
--   -> Located (Trail v n)
--   -> m -- ^ start
--   -> (Point v n, m) -- ^ result
-- foldTrail sf cf (Loc p0 t) m = case t of
--   Trail (ULine (Line s))   -> foldSegments sf cf p0 s m
--   Trail (ULoop (Line s) o) ->
--     let (p, m') = foldSegments sf cf p0 s m
--     in  case o of
--           Linear OffsetOpen      -> (p0, m' <> sf p (p .-. p0))
--           Cubic v1 v2 OffsetOpen -> (p0, m' <> cf p v1 v2 (p .-. p0))
-- {-# INLINE foldTrail #-}

-- instance (U.Unbox n, OrderedField n)
--   => HasQuery (Located (Trail V2 n)) Crossings where
--   getQuery t = Query $ \p -> trailCrossings p t
--   {-# INLINE getQuery #-}

-- instance (U.Unbox n, OrderedField n) => Traced (Trail V2 n) where
--   getTrace t = mkTrace $ trailTrace (t `at` origin)

------------------------------------------------------------------------
-- From trail
------------------------------------------------------------------------

class FromTrail t where
  -- | Make a @t@ from a trail.
  fromTrail :: Located (Trail (V t) (N t)) -> t

instance FromTrail t => FromTrail (Located t) where
  fromTrail t = fromTrail t `at` loc t
  {-# INLINE fromTrail #-}

instance Unboxable v n => FromTrail (Trail v n) where
  fromTrail = unLoc
  {-# INLINE fromTrail #-}

fromSegments :: (InSpace v n t, FromTrail t, Unboxable v n) => [Segment v n] -> t
fromSegments segs = fromTrail (OpenTrail (mkLine segs) `at` origin)

instance Unboxable v n => FromTrail (Line v n) where
  fromTrail = withTrail id cutLoop . unLoc
  {-# INLINE fromTrail #-}

-- instance Unboxable v n => FromTrail (Loop v n) where
--   fromTrail = withTrail glueLine id . unLoc
--   {-# INLINE fromTrail #-}

-- instance (Metric v, OrderedField n) => FromTrail [v n] where
--   fromTrail = toListOf (segments . to offset)
--   {-# INLINE fromTrail #-}

instance Unboxable v n => FromTrail [Point v n] where
  fromTrail (viewLoc -> (p0, t)) = scanl (.+^) p0 offsets where
    offsets = toListOf (segments . to offset) t
  {-# INLINE fromTrail #-}

-- -- | Construct a trail-like thing from a list of segments, with the
-- --   origin as the location.
-- --
-- --   <<diagrams/src_Diagrams_FromTrail_fromSegmentsEx.svg#diagram=fromSegmentsEx&height=200>>
-- --
-- --   > fromSegmentsEx = fromSegments
-- --   >   [ straight (r2 (1,1))
-- --   >   , bézier3  (r2 (1,1)) unitX unit_Y
-- --   >   , straight unit_X
-- --   >   ]
-- --   >   # centerXY # pad 1.1
-- fromSegments :: FromTrail t => [Segment Closed (V t) (N t)] -> t
-- fromSegments = fromLocSegments . (`at` origin)

-- -- | Construct a trail-like thing from a located list of segments.
-- fromLocSegments :: FromTrail t => Located [Segment Closed (V t) (N t)] -> t
-- fromLocSegments = fromTrail . mapLoc trailFromSegments

-- -- | Construct a trail-like thing of linear segments from a list
-- --   of offsets, with the origin as the location.
-- --
-- --   <<diagrams/src_Diagrams_FromTrail_fromOffsetsEx.svg#diagram=fromOffsetsEx&width=300>>
-- --
-- --   > fromOffsetsEx = fromOffsets
-- --   >   [ unitX
-- --   >   , unitX # rotateBy (1/6)
-- --   >   , unitX # rotateBy (-1/6)
-- --   >   , unitX
-- --   >   ]
-- --   >   # centerXY # pad 1.1
-- fromOffsets :: FromTrail t => [Vn t] -> t
-- fromOffsets = fromTrail . (`at` origin) . trailFromOffsets

-- -- | Construct a trail-like thing of linear segments from a located
-- --   list of offsets.
-- fromLocOffsets :: (V t ~ v, N t ~ n, V (v n) ~ v, N (v n) ~ n, FromTrail t) => Located [v n] -> t
-- fromLocOffsets = fromTrail . mapLoc trailFromOffsets

-- -- | Construct a trail-like thing connecting the given vertices with
-- --   linear segments, with the first vertex as the location.  If no
-- --   vertices are given, the empty trail is used with the origin as
-- --   the location.
-- --
-- --   <<diagrams/src_Diagrams_FromTrail_fromVerticesEx.svg#diagram=fromVerticesEx&width=300>>
-- --
-- --   > import Data.List (transpose)
-- --   >
-- --   > fromVerticesEx =
-- --   >   ( [ pentagon 1
-- --   >     , pentagon 1.3 # rotateBy (1/15)
-- --   >     , pentagon 1.5 # rotateBy (2/15)
-- --   >     ]
-- --   >     # transpose
-- --   >     # concat
-- --   >   )
-- --   >   # fromVertices
-- --   >   # closeTrail # strokeTrail
-- --   >   # centerXY # pad 1.1
-- fromVertices :: FromTrail t => [Point (V t) (N t)] -> t
-- fromVertices []       = fromTrail (emptyTrail `at` origin)
-- fromVertices ps@(p:_) = fromTrail (trailFromSegments (segmentsFromVertices ps) `at` p)

-- segmentsFromVertices :: (Additive v, Num n) => [Point v n] -> [Segment Closed v n]
-- segmentsFromVertices []         = []
-- segmentsFromVertices vvs@(_:vs) = map straight (zipWith (flip (.-.)) vvs vs)

-- -- | Create a linear trail between two given points.
-- --
-- --   <<diagrams/src_Diagrams_FromTrail_twiddleEx.svg#diagram=twiddleEx&width=300>>
-- --
-- --   > twiddleEx
-- --   >   = mconcat ((~~) <$> hexagon 1 <*> hexagon 1)
-- --   >   # centerXY # pad 1.1
-- (~~) :: (InSpace v n t, FromTrail t) => Point v n -> Point v n -> t
-- p1 ~~ p2 = fromVertices [p1, p2]

-- -- | Given a concretely located trail, \"explode\" it by turning each
-- --   segment into its own separate trail.  Useful for (say) applying a
-- --   different style to each segment.
-- --
-- --   <<diagrams/src_Diagrams_FromTrail_explodeTrailEx.svg#diagram=explodeTrailEx&width=300>>
-- --
-- --   > explodeTrailEx
-- --   >   = pentagon 1
-- --   >   # explodeTrail  -- generate a list of diagrams
-- --   >   # zipWith lc [orange, green, yellow, red, blue]
-- --   >   # mconcat # centerXY # pad 1.1
-- explodeTrail :: (InSpace v n t, FromTrail t) => Located (Trail v n) -> [t]
-- explodeTrail = map (mkTrail . view fixed) . fixTrail
--   where
--     mkTrail = fromTrail . mapLoc (trailFromSegments . (:[]))

