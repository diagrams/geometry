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

-- import           Control.Applicative       (liftA2)
import           Control.Lens              hiding (at, transform)
-- import qualified Data.Foldable             as F
-- import           Data.Semigroup

-- import           Data.Default.Class

-- import           Geometry.Angle
-- import           Geometry.Combinators      (withEnvelope, withTrace)
import           Geometry.Space
-- import           Geometry.Trace
-- import           Geometry.Transform
import           Geometry.Located          (Located, unLoc)
-- import           Geometry.Parametric
import           Geometry.Path
-- import           Geometry.Query
import           Geometry.Segment
-- import           Diagrams.Solve.Polynomial
import           Geometry.Trail
import           Geometry.TwoD.Segment
import           Geometry.TwoD.Types
-- import           Geometry.TwoD.Vector

-- import           Linear.Affine
-- import           Linear.Vector

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
  a <- toListOf each as
  b <- toListOf each bs
  intersectPointsT' eps a b

-- | Compute the intersect points between two located trails.
intersectPointsT :: OrderedField n => Located (Trail V2 n) -> Located (Trail V2 n) -> [P2 n]
intersectPointsT = intersectPointsT' 1e-8

-- | Compute the intersect points between two located trails within the given
--   tolerance.
intersectPointsT' :: OrderedField n => n -> Located (Trail V2 n) -> Located (Trail V2 n) -> [P2 n]
intersectPointsT' eps as bs = do
  a <- undefined -- fixTrail as
  b <- undefined -- fixTrail bs
  intersectPointsS' eps a b
