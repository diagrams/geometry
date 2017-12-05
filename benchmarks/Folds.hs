{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Benchmarks for folds over paths. This includes things like
--   envelopes and traces.

import Criterion.Main
import Control.DeepSeq

import Geometry

import Samples

main :: IO ()
main = do
  let path10 = toPath . take 10
  let !dragon8  = fromOffsets (dragonOffsets 8) :: Path V2 Double
      !squares10 = path10 (randomSquares 0)
      !rects10   = path10 (randomRects 1)
      !circles10 = path10 (randomCircles 2)
      !ellipses10 = path10 (randomEllipses 3)
      !triangles10 = path10 (randomTriangles 4)
  let trace     = traceV origin (V2 0.4 0.7)
  let crossings = \t -> sample t origin
  defaultMain
    [ bgroup "bounding-box"
      [ bench "dragon-8"     $ whnf (diameter unitX) dragon8
      , bench "squares-10"   $ whnf (diameter unitX) squares10
      , bench "rects-10"     $ whnf (diameter unitX) rects10
      , bench "circles-10"   $ whnf (diameter unitX) circles10
      , bench "ellipses-10"  $ whnf (diameter unitX) ellipses10
      , bench "triangles-10" $ whnf (diameter unitX) triangles10
      ]
    , bgroup "trace"
      [ bench "dragon-8"     $ nf trace dragon8
      , bench "squares-10"   $ nf trace squares10
      , bench "rects-10"     $ nf trace rects10
      , bench "circles-10"   $ nf trace circles10
      , bench "ellipses-10"  $ nf trace ellipses10
      , bench "triangles-10" $ nf trace triangles10
      ]
    , bgroup "crossings"
      [ bench "dragon-8"     $ whnf crossings dragon8
      , bench "squares-10"   $ whnf crossings squares10
      , bench "rects-10"     $ whnf crossings rects10
      , bench "circles-10"   $ whnf crossings circles10
      , bench "ellipses-10"  $ whnf crossings ellipses10
      , bench "triangles-10" $ whnf crossings triangles10
      ]
    ]

