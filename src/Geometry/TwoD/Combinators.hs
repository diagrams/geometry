{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.TwoD.Combinators
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
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
  , hcat, hsep, hsepEven
  , vcat, vsep, vsepEven

    -- ** Align by envelope
  , alignL, alignR, alignT, alignB
  , alignTL, alignTR, alignBL, alignBR

    -- ** Align by trace
  , snugL, snugR, snugT, snugB

    -- * Relative alignment
  , alignX, snugX, alignY, snugY

    -- * Centering
  , centerX, centerY, centerXY
  , snugCenterX, snugCenterY, snugCenterXY


  ) where

import           Data.Semigroup

import           Geometry.Combinators
import           Geometry.Envelope
import           Geometry.HasOrigin
import           Geometry.Juxtapose
import           Geometry.Space
import           Geometry.Trace
import           Geometry.TwoD.Types
import           Geometry.TwoD.Vector

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
(===) = atDirection y_Dir

-- | Place two diagrams (or other juxtaposable objects) horizontally
--   adjacent to one another, with the first diagram to the left of
--   the second.  The local origin of the resulting combined diagram
--   is the same as the local origin of the first.  @(|||)@ is
--   associative and has 'mempty' as an identity.  See the
--   documentation of 'beside' for more information.
(|||) :: (InSpace V2 n a, Juxtaposable a, Semigroup a) => a -> a -> a
(|||) = atDirection xDir

-- | Lay out a list of enveloped objects in a row from left to right,
--   so that their local origins lie along a single horizontal line,
--   with successive envelopes tangent to one another.
--
--   * To /align/ vertically (or otherwise), use alignment
--     combinators (such as 'alignT' or 'alignB') before applying
--     'hcat'.
--
--   * For non-axis-aligned layout, see 'cat'.
hcat :: (R1 v, InSpace v n a, Enveloped a, HasOrigin a, Monoid a)
     => [a] -> a
hcat = cat unitX

-- | A convenient synonym for horizontal concatenation with
--   separation: @'hsep' = 'sep' 'unitX'@.
hsep :: (R1 v, InSpace v n a, Enveloped a, HasOrigin a, Monoid a)
     => n -> [a] -> a
hsep = sep unitX

-- | A convenient synonym for horizontal even separation: @'hsepEven' =
--   'sepEven' 'unitX'@.
hsepEven
  :: (R1 v, InSpace v n a, Enveloped a, HasOrigin a, Monoid a)
  => n -> [a] -> a
hsepEven = sepEven unitX

-- | Lay out a list of juxtaposable objects in a column from top to
--   bottom, so that their local origins lie along a single vertical
--   line, with successive envelopes tangent to one another.
--
--   * To align the diagrams horizontally (or otherwise), use alignment
--     combinators (such as 'alignL' or 'alignR') before applying
--     'vcat'.
--
--   * For non-axis-aligned layout, see 'cat'.
vcat :: (R2 v, InSpace v n a, Enveloped a, HasOrigin a, Monoid a)
     => [a] -> a
vcat = cat unit_Y

-- | A convenient synonym for vertical concatenation from top to bottom
--   with separation: @'vsep' = 'sep' 'unit_Y'@.
vsep :: (R2 v, InSpace v n a, Enveloped a, HasOrigin a, Monoid a)
     => n -> [a] -> a
vsep = sep unit_Y

-- | A convenient synonym for horizontal even separation from top to
-- bottom: @'hsepEven' = 'sepEven' 'unit_Y'@.
vsepEven
  :: (R2 v, InSpace v n a, Enveloped a, HasOrigin a, Monoid a)
  => n -> [a] -> a
vsepEven = sepEven unit_Y

------------------------------------------------------------------------
-- Alignment
------------------------------------------------------------------------

-- | Align along the left edge, i.e. translate the diagram in a
--   horizontal direction so that the local origin is on the left edge
--   of the envelope.
alignL :: (InSpace V2 n a, Enveloped a, HasOrigin a) => a -> a
alignL = align unit_X

snugL :: (InSpace V2 n a, Fractional n, Traced a, HasOrigin a) => a -> a
snugL = snug unit_X

-- | Align along the right edge.
alignR
  :: (InSpace V2 n a, Enveloped a, HasOrigin a)
  => a -> a
alignR = align unitX

snugR
  :: (InSpace V2 n a, Fractional n, Traced a, HasOrigin a)
  => a -> a
snugR = snug unitX


-- | Align along the top edge.
alignT
  :: (InSpace V2 n a, Enveloped a, HasOrigin a)
  => a -> a
alignT = align unitY

snugT
  :: (InSpace V2 n a, Fractional n, Traced a, HasOrigin a)
  => a -> a
snugT = snug unitY

-- | Align along the bottom edge.
alignB
  :: (InSpace V2 n a, Enveloped a, HasOrigin a)
  => a -> a
alignB = align unit_Y

snugB
  :: (InSpace V2 n a, Fractional n, Traced a, HasOrigin a)
  => a -> a
snugB = snug unit_Y

alignTL, alignTR, alignBL, alignBR
  :: (InSpace V2 n a, Enveloped a, HasOrigin a)
  => a -> a
alignTL = alignT . alignL
alignTR = alignT . alignR
alignBL = alignB . alignL
alignBR = alignB . alignR

-- | @alignX@ and @snugX@ move the local origin horizontally as follows:
--
--   * @alignX (-1)@ moves the local origin to the left edge of the boundary;
--
--   * @align 1@ moves the local origin to the right edge;
--
--   * any other argument interpolates linearly between these.  For
--     example, @alignX 0@ centers, @alignX 2@ moves the origin one
--     \"radius\" to the right of the right edge, and so on.
--
--   * @snugX@ works the same way.

alignX
  :: (InSpace v n a, R1 v, Enveloped a, HasOrigin a)
  => n -> a -> a
alignX = alignBy unitX

-- | See the documentation for 'alignX'.
snugX
  :: (InSpace v n a, R1 v, Fractional n, Traced a, HasOrigin a)
  => n -> a -> a
snugX = snugBy unitX

-- | Like 'alignX', but moving the local origin vertically, with an
--   argument of @1@ corresponding to the top edge and @(-1)@ corresponding
--   to the bottom edge.
alignY
  :: (InSpace v n a, R2 v, Enveloped a, HasOrigin a)
  => n -> a -> a
alignY = alignBy unitY

-- | See the documentation for 'alignY'.
snugY
  :: (InSpace v n a, R2 v, Fractional n, Traced a, HasOrigin a)
  => n -> a -> a
snugY = snugBy unitY

-- | Center the local origin along the X-axis.
centerX
  :: (InSpace v n a, R1 v, Enveloped a, HasOrigin a)
  => a -> a
centerX = alignBy unitX 0

snugCenterX
  :: (InSpace v n a, R1 v, Fractional n, Traced a, HasOrigin a)
  => a -> a
snugCenterX = snugBy unitX 0

-- | Center the local origin along the Y-axis.
centerY
  :: (InSpace v n a, R2 v, Enveloped a, HasOrigin a)
  => a -> a
centerY = alignBy unitY 0

snugCenterY
  :: (InSpace v n a, R2 v, Fractional n, Traced a, HasOrigin a)
  => a -> a
snugCenterY = snugBy unitY 0

-- | Center along both the X- and Y-axes.
centerXY
  :: (InSpace v n a, R2 v, Enveloped a, HasOrigin a)
  => a -> a
centerXY = centerX . centerY

snugCenterXY
  :: (InSpace v n a, R2 v, Fractional n, Traced a, HasOrigin a)
  => a -> a
snugCenterXY = snugCenterX . snugCenterY

