{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Samples
  ( dragonOffsets
  , randomSquares
  , randomRects
  , randomCircles
  , randomEllipses
  , randomTriangles
  ) where

import Geometry hiding (P)
import System.Random.PCG.Pure
import System.Random.PCG.Class
import Data.Word

(#) :: a -> (a -> b) -> b
a # f = f a
infixl 8 #

-- | Infinite list of random doubles given some seed.
floats :: Word64 -> [Double]
floats seed = go (initFrozen 0xdcb850c5b9d7eeba seed) where
  go g = realToFrac (wordToFloat r) : go g'
    where
    (r, g') = next' g

------------------------------------------------------------------------
-- Generated
------------------------------------------------------------------------

-- Dragon curve --------------------------------------------------------

data Tok = F | P | M | X | Y

rewriteFunction :: Tok -> [Tok]
rewriteFunction X = [X, P, Y, F, P]
rewriteFunction Y = [M, F, X, M, Y]
rewriteFunction t = [t]
{-# INLINE rewriteFunction #-}

gens :: [[Tok]]
gens = iterate (concatMap rewriteFunction) [F, X]
{-# INLINE gens #-}

toks2offsets :: [Tok] -> [V2 Double]
toks2offsets xs = [v | (Just v, _) <- scanl f (Nothing, unitX) xs] where
  f (_, dir) F = (Just dir, dir)
  f (_, dir) P = (Nothing, perp dir)
  f (_, dir) M = (Nothing, negate $ perp dir)
  f (_, dir) _ = (Nothing, dir)
{-# INLINE toks2offsets #-}

dragonOffsets :: Int -> [V2 Double]
dragonOffsets n = toks2offsets $ gens !! n
{-# INLINE dragonOffsets #-}

------------------------------------------------------------------------
-- Random Shapes
------------------------------------------------------------------------

-- Squares -------------------------------------------------------------

randomSquares :: Word64 -> [Located (Trail V2 Double)]
randomSquares = go . floats where
  go (r1:r2:r3:rs) = (square w `at` mkP2 x y) : go rs
    where
    w = r1 * 10 + 0.5
    x = r2 * 10
    y = r3 * 10
{-# INLINE randomSquares #-}

randomRects :: Word64 -> [Located (Trail V2 Double)]
randomRects = go . floats where
  go (r1:r2:r3:r4:rs) = (rect w h `at` mkP2 x y) : go rs
    where
    w = r1 * 10 + 0.1
    h = r2 * 10 + 0.1
    x = r3 * 100 - 50
    y = r4 * 100 - 50
{-# INLINE randomRects #-}

-- Circles -------------------------------------------------------------

randomCircles :: Word64 -> [Located (Trail V2 Double)]
randomCircles = go . floats where
  go (r1:r2:r3:rs) = (circle r `at` mkP2 x y) : go rs
    where
    r = r1 * 10 + 0.5
    x = r2 * 100 - 50
    y = r3 * 100 - 50
{-# INLINE randomCircles #-}

randomEllipses :: Word64 -> [Located (Trail V2 Double)]
randomEllipses = go . floats where
  go (r1:r2:r3:r4:rs) = (ellipse e # scale r `at` mkP2 x y) : go rs
    where
    r = r1 * 10 + 0.1
    e = r2 * 0.9
    y = r3 * 100 - 50
    x = r4 * 100 - 50
{-# INLINE randomEllipses #-}

-- Triangles -----------------------------------------------------------

randomTriangles :: Word64 -> [Located (Trail V2 Double)]
randomTriangles seed = go (floats seed) where
  go (r1:r2:r3:r4:rs) = (triangle w # rotate a `at` mkP2 x y) : go rs
    where
    w = r1 * 10 + 0.1
    a = r2 @@ turn
    x = r3 * 100 - 50
    y = r4 * 100 - 50
{-# INLINE randomTriangles #-}

