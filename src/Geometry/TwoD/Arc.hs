{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.TwoD.Arc
-- Copyright   :  (c) 2011-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Two-dimensional arcs, approximated by cubic bezier curves.
--
-----------------------------------------------------------------------------

module Geometry.TwoD.Arc
  ( arc
  , arc'
  , arcT

  , arcCCW
  , arcCW

  , bezierFromSweep

  , wedge
  , arcBetween
  , annularWedge
  ) where

import qualified Data.Semigroup          as Sem
import           Geometry.Angle
import           Geometry.Direction
import           Geometry.Located        (at)
import           Geometry.Segment
import           Geometry.Space
import           Geometry.Trail
import           Geometry.Transform
import           Geometry.TwoD.Transform
import           Geometry.TwoD.Types
import           Geometry.TwoD.Vector    (e, unitX, unitY, unit_Y)

import           Control.Lens            (each, over, reversing, (&), (<>~),
                                          (^.))

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

-- For details of this approximation see:
--   http://www.tinaja.com/glib/bezcirc2.pdf

-- | @bezierFromSweepQ1 s@ constructs a 'Cubic' segment that starts in
--   the positive y direction and sweeps counterclockwise through an
--   angle @s@.  The approximation is only valid for angles in the
--   first quadrant.
bezierFromSweepQ1 :: Floating n => Angle n -> Segment V2 n
bezierFromSweepQ1 s = over each (^-^ unitX) . rotate (s ^/ 2) $ bezier3 c2 c1 p0
  where p0@(V2 x y) = e (s ^/ 2)
        c1          = V2 ((4-x)/3) ((1-x)*(3-x)/(3*y))
        c2          = reflectY c1

-- | @bezierFromSweep s@ constructs a series of 'Cubic' segments that
--   start in the positive y direction and sweep counter clockwise
--   through the angle @s@.  If @s@ is negative, it will start in the
--   negative y direction and sweep clockwise.  When @s@ is less than
--   0.0001 the empty list results.  If the sweep is greater than @fullTurn@
--   later segments will overlap earlier segments.
bezierFromSweep :: OrderedField n => Angle n -> [Segment V2 n]
bezierFromSweep s
  | s < zero          = fmap reflectY . bezierFromSweep $ negated s
  | s < 0.0001 @@ rad = []
  | s < fullTurn^/4   = [bezierFromSweepQ1 s]
  | otherwise         = bezierFromSweepQ1 (fullTurn^/4)
          : map (rotateBy (1/4)) (bezierFromSweep (max (s ^-^ fullTurn^/4) zero))

{-
~~~~ Note [segment spacing]

There are a few obvious options for segment spacing:
   A. Evenly space segments each with sweep less than or equal
      to one quarter of a circle.  This has the benefit of a better approximation
      (at least I think it is better).
   B. Use as much of the sweep in quarter-circle sized segments and one for
      the remainder.  This potentially gives more opportunities for
      consistency (though not as much as option C) as the error in
      approximation would more often match the error from another arc
      in the diagram.
   C. Like option B but fixing the orientation and having a remnant at
      the beginning and the end.

Option B is implemented and this note is for posterity if anyone comes
across a situation with large enough arcs that they can actually see
the approximation error.
-}

-- | Given a start direction @d@ and a sweep angle @s@, @'arcT' d s@
--   is the 'Trail' of a radius one arc starting at @d@ and sweeping out
--   the angle @s@ counterclockwise (for positive s).  The resulting
--   @Trail@ is allowed to wrap around and overlap itself.
arcT :: OrderedField n => Direction V2 n -> Angle n -> Trail V2 n
arcT start sweep = fromSegments bs
  where
    bs = map (rotateTo start) . bezierFromSweep $ sweep

-- | Given a start direction @d@ and a sweep angle @s@, @'arc' d s@ is the
--   path of a radius one arc starting at @d@ and sweeping out the angle
--   @s@ counterclockwise (for positive s).  The resulting
--   @Trail@ is allowed to wrap around and overlap itself.
arc :: (InSpace V2 n t, OrderedField n, FromTrail t) => Direction V2 n -> Angle n -> t
arc start sweep = fromLocTrail $ arcT start sweep `at` P (fromDirection start)

-- | Given a radus @r@, a start direction @d@ and an angle @s@,
--   @'arc'' r d s@ is the path of a radius @(abs r)@ arc starting at
--   @d@ and sweeping out the angle @s@ counterclockwise (for positive
--   s).  The origin of the arc is its center.
--
--   <<diagrams/src_Geometry_TwoD_Arc_arc'Ex.svg#diagram=arc'Ex&width=300>>
--
--   > arc'Ex = mconcat [ arc' r xDir (1/4 @@ turn) | r <- [0.5,-1,1.5] ]
--   >        # centerXY # pad 1.1
arc' :: (InSpace V2 n t, OrderedField n, FromTrail t) => n -> Direction V2 n -> Angle n -> t
arc' (abs -> r) start sweep = fromLocTrail $ scale r ts `at` P (r *^ fromDirection start)
  where ts = arcT start sweep

arcCCWT :: RealFloat n => Direction V2 n -> Direction V2 n -> Trail V2 n
arcCCWT start end = fromSegments bs
  where
    bs    = map (rotateTo start) . bezierFromSweep $ sweep
    sweep = normalizeAngle $ end ^. _theta ^-^ start ^. _theta

-- | Given a start direction @s@ and end direction @e@, @arcCCW s e@ is the
--   path of a radius one arc counterclockwise between the two directions.
--   The origin of the arc is its center.
arcCCW :: (InSpace V2 n t, RealFloat n, FromTrail t) => Direction V2 n -> Direction V2 n -> t
arcCCW start end = fromLocTrail $ arcCCWT start end `at` P (fromDirection start)

-- | Like 'arcAngleCCW' but clockwise.
arcCW :: (InSpace V2 n t, RealFloat n, FromTrail t) => Direction V2 n -> Direction V2 n -> t
arcCW start end = fromLocTrail $
  -- flipped arguments to get the path we want
  -- then reverse the trail to get the cw direction.
  reversing (arcCCWT end start) `at` P (fromDirection start)

-- | Create a circular wedge of the given radius, beginning at the
--   given direction and extending through the given angle.
--
--   <<diagrams/src_Geometry_TwoD_Arc_wedgeEx.svg#diagram=wedgeEx&width=400>>
--
--   > wedgeEx = hsep 0.5
--   >   [ wedge 1 xDir (1/4 @@ turn)
--   >   , wedge 1 (rotate (7/30 @@ turn) xDir) (4/30 @@ turn)
--   >   , wedge 1 (rotate (1/8 @@ turn) xDir) (3/4 @@ turn)
--   >   ]
--   >   # fc blue
--   >   # centerXY # pad 1.1
wedge :: (InSpace V2 n t, OrderedField n, FromTrail t) => n -> Direction V2 n -> Angle n -> t
wedge r d s = fromLocTrail . (`at` origin) . glueTrail . wrapLine
              $ fromOffsets [r *^ fromDirection d]
            Sem.<> scale r (arc d s)
            Sem.<> fromOffsets [r *^ negated (rotate s $ fromDirection d)]

-- | @arcBetween p q height@ creates an arc beginning at @p@ and
--   ending at @q@, with its midpoint at a distance of @abs height@
--   away from the straight line from @p@ to @q@.  A positive value of
--   @height@ results in an arc to the left of the line from @p@ to
--   @q@; a negative value yields one to the right.
--
--   <<diagrams/src_Geometry_TwoD_Arc_arcBetweenEx.svg#diagram=arcBetweenEx&width=300>>
--
--   > arcBetweenEx = mconcat
--   >   [ arcBetween origin (p2 (2,1)) ht | ht <- [-0.2, -0.1 .. 0.2] ]
--   >   # centerXY # pad 1.1
arcBetween :: (InSpace V2 n t, FromTrail t, RealFloat n) => Point V2 n -> Point V2 n -> n -> t
arcBetween p q ht = fromLocTrail (a & rotate (v^._theta) & moveTo p)
  where
    h = abs ht
    isStraight = h < 0.00001
    v = q .-. p
    d = norm (q .-. p)
    th  = acosA ((d*d - 4*h*h)/(d*d + 4*h*h))
    r = d/(2*sinA th)
    mid | ht >= 0    = direction unitY
        | otherwise  = direction unit_Y
    st  = mid & _theta <>~ negated th
    a | isStraight
      = fromOffsets [d *^ unitX]
      | otherwise
      = arc st (2 *^ th)
        & scale r
        & translateY ((if ht > 0 then negate else id) (r - h))
        & translateX (d/2)
        & (if ht > 0 then reversing else id)

-- | Create an annular wedge of the given radii, beginning at the
--   first direction and extending through the given sweep angle.
--   The radius of the outer circle is given first.
--
--   <<diagrams/src_Geometry_TwoD_Arc_annularWedgeEx.svg#diagram=annularWedgeEx&width=400>>
--
--   > annularWedgeEx = hsep 0.50
--   >   [ annularWedge 1 0.5 xDir (1/4 @@ turn)
--   >   , annularWedge 1 0.3 (rotate (7/30 @@ turn) xDir) (4/30 @@ turn)
--   >   , annularWedge 1 0.7 (rotate (1/8 @@ turn) xDir) (3/4 @@ turn)
--   >   ]
--   >   # fc blue
--   >   # centerXY # pad 1.1
annularWedge :: (InSpace V2 n t, FromTrail t, RealFloat n) =>
                n -> n -> Direction V2 n -> Angle n -> t
annularWedge r1' r2' d1 s = fromLocTrail . (`at` o) . glueTrail . wrapLine
              $ fromOffsets [(r1' - r2') *^ fromDirection d1]
            Sem.<> scale r1' (arc d1 s)
            Sem.<> fromOffsets [(r1' - r2') *^ negated (fromDirection d2)]
            Sem.<> scale r2' (arc d2 (negated s))
  where o = P (r2' *^ fromDirection d1)
        d2 = d1 & _theta <>~ s

