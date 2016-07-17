{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Path
-- Copyright   :  (c) 2011-2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Paths in two dimensions are special since we may stroke them to
-- create a 2D diagram, and (eventually) perform operations such as
-- intersection and union.  They also have a trace, whereas paths in
-- higher dimensions do not.
--
-----------------------------------------------------------------------------

module Geometry.TwoD.Path
  (
    -- ** Inside/outside testing

    Crossings (..)
  , isInsideWinding
  , isInsideEvenOdd

    -- * Intersections

  , intersectPoints, intersectPoints'
  , intersectPointsP, intersectPointsP'
  , intersectPointsT, intersectPointsT'
  ) where

import           Control.Applicative       (liftA2)
import           Control.Lens              hiding (at, transform)
import qualified Data.Foldable             as F
import           Data.Semigroup

-- import           Data.Default.Class

import           Geometry.Angle
-- import           Geometry.Combinators      (withEnvelope, withTrace)
import           Geometry.Space
import           Geometry.Trace
import           Geometry.Transform
import           Geometry.Located          (Located, mapLoc, unLoc)
import           Geometry.Parametric
import           Geometry.Path
import           Geometry.Query
import           Geometry.Segment
import           Diagrams.Solve.Polynomial
import           Geometry.Trail
import           Geometry.TwoD.Segment
import           Geometry.TwoD.Types
import           Geometry.TwoD.Vector

import           Linear.Affine
import           Linear.Vector

------------------------------------------------------------------------
-- Trail and path traces
------------------------------------------------------------------------

-- Only 2D trails and paths have a trace.

-- XXX can the efficiency of this be improved?  See the comment in
-- Diagrams.Path on the Enveloped instance for Trail.
instance RealFloat n => Traced (Trail V2 n) where
  getTrace = withLine $
      foldr
        (\seg bds -> moveOriginBy (negated . atEnd $ seg) bds <> getTrace seg)
        mempty
    . lineSegments

instance RealFloat n => Traced (Path V2 n) where
  getTrace = F.foldMap getTrace . op Path

------------------------------------------------------------------------
-- Constructing path-based diagrams
------------------------------------------------------------------------

------------------------------------------------------------
--  Inside/outside testing
------------------------------------------------------------

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
-- 'sample' :: 'Path' 'V2' 'Double'                  -> 'Point' 'V2' 'Double' -> 'Crossings'
-- 'sample' :: 'Located' ('Trail' 'V2' 'Double')       -> 'Point' 'V2' 'Double' -> 'Crossings'
-- 'sample' :: 'Located' ('Trail'' 'Loop' 'V2' 'Double') -> 'Point' 'V2' 'Double' -> 'Crossings'
-- @
--
--   Note that 'Line's have no inside or outside, so don't contribute
--   crossings
newtype Crossings = Crossings Int
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

instance Semigroup Crossings where
  Crossings a <> Crossings b = Crossings (a + b)

instance Monoid Crossings where
  mempty  = Crossings 0
  mappend = (<>)

instance RealFloat n => HasQuery (Located (Trail V2 n)) Crossings where
  getQuery trail = Query $ \p -> trailCrossings p trail

instance RealFloat n => HasQuery (Located (Trail' l V2 n)) Crossings where
  getQuery trail' = getQuery (mapLoc Trail trail')

instance RealFloat n => HasQuery (Path V2 n) Crossings where
  getQuery = foldMapOf each getQuery

-- | Test whether the given point is inside the given path,
--   by testing whether the point's /winding number/ is nonzero. Note
--   that @False@ is /always/ returned for paths consisting of lines
--   (as opposed to loops), regardless of the winding number.
--
-- @
-- 'isInsideWinding' :: 'Path' 'V2' 'Double'                  -> 'Point' 'V2' 'Double' -> 'Bool'
-- 'isInsideWinding' :: 'Located' ('Trail' 'V2' 'Double')       -> 'Point' 'V2' 'Double' -> 'Bool'
-- 'isInsideWinding' :: 'Located' ('Trail'' 'Loop' 'V2' 'Double') -> 'Point' 'V2' 'Double' -> 'Bool'
-- @
isInsideWinding :: HasQuery t Crossings => t -> Point (V t) (N t) -> Bool
isInsideWinding t = (/= 0) . sample t

-- | Test whether the given point is inside the given path,
--   by testing whether a ray extending from the point in the positive
--   x direction crosses the path an even (outside) or odd (inside)
--   number of times.  Note that @False@ is /always/ returned for
--   paths consisting of lines (as opposed to loops), regardless of
--   the number of crossings.
--
-- @
-- 'isInsideEvenOdd' :: 'Path' 'V2' 'Double'                  -> 'Point' 'V2' 'Double' -> 'Bool'
-- 'isInsideEvenOdd' :: 'Located' ('Trail' 'V2' 'Double')       -> 'Point' 'V2' 'Double' -> 'Bool'
-- 'isInsideEvenOdd' :: 'Located' ('Trail'' 'Loop' 'V2' 'Double') -> 'Point' 'V2' 'Double' -> 'Bool'
-- @
isInsideEvenOdd :: HasQuery t Crossings => t -> Point (V t) (N t) -> Bool
isInsideEvenOdd t = odd . sample t

-- | Compute the sum of signed crossings of a trail starting from the
--   given point in the positive x direction.
trailCrossings :: RealFloat n => Point V2 n -> Located (Trail V2 n) -> Crossings

  -- non-loop trails have no inside or outside, so don't contribute crossings
trailCrossings _ t | not (isLoop (unLoc t)) = 0

trailCrossings p@(unp2 -> (x,y)) tr
  = F.foldMap test $ fixTrail tr
  where
    test (FLinear a@(unp2 -> (_,ay)) b@(unp2 -> (_,by)))
      | ay <= y && by > y && isLeft a b > 0 =  1
      | by <= y && ay > y && isLeft a b < 0 = -1
      | otherwise                           =  0

    test c@(FCubic (P x1@(V2 _ x1y))
                   (P c1@(V2 _ c1y))
                   (P c2@(V2 _ c2y))
                   (P x2@(V2 _ x2y))
           ) =
        sum . map testT $ ts
      where ts = filter (liftA2 (&&) (>=0) (<=1))
               $ cubForm (-  x1y + 3*c1y - 3*c2y + x2y)
                         ( 3*x1y - 6*c1y + 3*c2y)
                         (-3*x1y + 3*c1y)
                         (x1y - y)
            testT t = let (unp2 -> (px,_)) = c `atParam` t
                      in  if px > x then signFromDerivAt t else 0
            signFromDerivAt t =
              let v =  (3*t*t) *^ ((-1)*^x1 ^+^ 3*^c1 ^-^ 3*^c2 ^+^ x2)
                   ^+^ (2*t)   *^ (3*^x1 ^-^ 6*^c1 ^+^ 3*^c2)
                   ^+^            ((-3)*^x1 ^+^ 3*^c1)
                  ang = v ^. _theta . rad
              in  case () of _ | 0      < ang && ang < tau/2 && t < 1 ->  1
                               | -tau/2 < ang && ang < 0     && t > 0 -> -1
                               | otherwise                            ->  0

    isLeft a b = cross2 (b .-. a) (p .-. a)

    tau = 2*pi

------------------------------------------------------------
--  Intersections  -----------------------------------------
------------------------------------------------------------

-- | Find the intersect points of two objects that can be converted to a path.
intersectPoints :: (InSpace V2 n t, SameSpace t s, ToPath t, ToPath s, OrderedField n)
  => t -> s -> [P2 n]
intersectPoints = intersectPoints' 1e-8

-- | Find the intersect points of two objects that can be converted to a path
--   within the given tolerance.
intersectPoints' :: (InSpace V2 n t, SameSpace t s, ToPath t, ToPath s, OrderedField n)
  => n -> t -> s -> [P2 n]
intersectPoints' eps t s = intersectPointsP' eps (toPath t) (toPath s)

-- | Compute the intersect points between two paths.
intersectPointsP :: OrderedField n => Path V2 n -> Path V2 n -> [P2 n]
intersectPointsP = intersectPointsP' 1e-8

-- | Compute the intersect points between two paths within given tolerance.
intersectPointsP' :: OrderedField n => n -> Path V2 n -> Path V2 n -> [P2 n]
intersectPointsP' eps as bs = do
  a <- pathTrails as
  b <- pathTrails bs
  intersectPointsT' eps a b

-- | Compute the intersect points between two located trails.
intersectPointsT :: OrderedField n => Located (Trail V2 n) -> Located (Trail V2 n) -> [P2 n]
intersectPointsT = intersectPointsT' 1e-8

-- | Compute the intersect points between two located trails within the given
--   tolerance.
intersectPointsT' :: OrderedField n => n -> Located (Trail V2 n) -> Located (Trail V2 n) -> [P2 n]
intersectPointsT' eps as bs = do
  a <- fixTrail as
  b <- fixTrail bs
  intersectPointsS' eps a b
