{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Combinators
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Diagram combinators specialized to two dimensions. For more general
-- combinators, see "Diagrams.Combinators".
--
-----------------------------------------------------------------------------

module Geometry.TwoD.Combinators
  (
    -- * Binary combinators

    (===), (|||)

    -- * n-ary combinators
  , hcat, hcat', hsep
  , vcat, vcat', vsep

    -- * Spacing/envelopes
  -- , strutR2
  -- , strutX, strutY
  -- , padX, padY

  -- , extrudeLeft, extrudeRight, extrudeBottom, extrudeTop

  -- , rectEnvelope

  -- , boundingRect, bg, bgFrame

  ) where

import           Control.Lens             ((&), (.~))
import           Data.Default.Class
import           Data.Semigroup

import Data.Monoid.WithSemigroup

import           Geometry.Space
-- import           Geometry.Transform

-- import           Geometry.Attributes      (lwO)
-- import           Geometry.BoundingBox
import           Geometry.Combinators
-- import           Geometry.Path
-- import           Geometry.Segment
import Geometry.HasOrigin
-- import           Geometry.TrailLike
-- import           Geometry.TwoD.Align
import           Geometry.Juxtapose
-- import           Geometry.TwoD.Attributes (fc)
-- import           Geometry.TwoD.Path       ()
-- import           Geometry.TwoD.Shapes
-- import           Geometry.TwoD.Transform  (scaleX, scaleY)
import           Geometry.TwoD.Types
import           Geometry.TwoD.Vector
-- import           Geometry.Util            (( # ))

-- import           Linear.Affine
-- import           Linear.Metric
-- import           Linear.Vector

infixl 6 ===
infixl 6 |||

-- | Place two diagrams (or other objects) vertically adjacent to one
--   another, with the first diagram above the second.  Since Haskell
--   ignores whitespace in expressions, one can thus write
--
--   @
--       c
--      ===
--       d
--   @
--
--   to place @c@ above @d@.  The local origin of the resulting
--   combined diagram is the same as the local origin of the first.
--   @(===)@ is associative and has 'mempty' as an identity.  See the
--   documentation of 'beside' for more information.
(===) :: (InSpace V2 n a, Juxtaposable a, Semigroup a) => a -> a -> a
(===) = beside unit_Y

-- | Place two diagrams (or other juxtaposable objects) horizontally
--   adjacent to one another, with the first diagram to the left of
--   the second.  The local origin of the resulting combined diagram
--   is the same as the local origin of the first.  @(|||)@ is
--   associative and has 'mempty' as an identity.  See the
--   documentation of 'beside' for more information.
(|||) :: (InSpace V2 n a, Juxtaposable a, Semigroup a) => a -> a -> a
(|||) = beside unitX

-- | Lay out a list of juxtaposable objects in a row from left to right,
--   so that their local origins lie along a single horizontal line,
--   with successive envelopes tangent to one another.
--
--   * For more control over the spacing, see 'hcat''.
--
--   * To align the diagrams vertically (or otherwise), use alignment
--     combinators (such as 'alignT' or 'alignB') from
--     "Diagrams.TwoD.Align" before applying 'hcat'.
--
--   * For non-axis-aligned layout, see 'cat'.
hcat :: (InSpace V2 n a, Floating n, Juxtaposable a, HasOrigin a, Monoid' a)
     => [a] -> a
hcat = hcat' def

-- | A variant of 'hcat' taking an extra 'CatOpts' record to control
--   the spacing.  See the 'cat'' documentation for a description of
--   the possibilities. For the common case of setting just a
--   separation amount, see 'hsep'.
hcat' :: (InSpace V2 n a, Floating n, Juxtaposable a, HasOrigin a, Monoid' a)
      => CatOpts n -> [a] -> a
hcat' = cat' unitX

-- | A convenient synonym for horizontal concatenation with
--   separation: @hsep s === hcat' (with & sep .~ s)@.
hsep :: (InSpace V2 n a, Floating n, Juxtaposable a, HasOrigin a, Monoid' a)
     => n -> [a] -> a
hsep s = hcat' (def & sep .~ s)

-- | Lay out a list of juxtaposable objects in a column from top to
--   bottom, so that their local origins lie along a single vertical
--   line, with successive envelopes tangent to one another.
--
--   * For more control over the spacing, see 'vcat''.
--
--   * To align the diagrams horizontally (or otherwise), use alignment
--     combinators (such as 'alignL' or 'alignR') from
--     "Diagrams.TwoD.Align" before applying 'vcat'.
--
--   * For non-axis-aligned layout, see 'cat'.
vcat :: (InSpace V2 n a, Floating n, Juxtaposable a, HasOrigin a, Monoid' a)
     => [a] -> a
vcat = vcat' def

-- | A variant of 'vcat' taking an extra 'CatOpts' record to control
--   the spacing.  See the 'cat'' documentation for a description of
--   the possibilities.  For the common case of setting just a
--   separation amount, see 'vsep'.
vcat' :: (InSpace V2 n a, Floating n, Juxtaposable a, HasOrigin a, Monoid' a)
      => CatOpts n -> [a] -> a
vcat' = cat' unit_Y

-- | A convenient synonym for vertical concatenation with
--   separation: @vsep s === vcat' (with & sep .~ s)@.
vsep :: (InSpace V2 n a, Floating n, Juxtaposable a, HasOrigin a, Monoid' a)
     => n -> [a] -> a
vsep s = vcat' (def & sep .~ s)

