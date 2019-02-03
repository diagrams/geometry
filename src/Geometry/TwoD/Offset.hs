{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.TwoD.Offset
-- Copyright   :  (c) 2013-2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Compute offsets to segments in two dimensions.  More details can be
-- found in the manual at
-- <http://projects.haskell.org/diagrams/doc/manual.html#offsets-of-segments-trails-and-paths>.
--
-----------------------------------------------------------------------------
module Geometry.TwoD.Offset
  (
    LineJoin (..)
  , LineCap (..)
    -- * Offsets

  , offsetSegment

  , OffsetOpts(..), offsetJoin, offsetMiterLimit, offsetEpsilon
  , offsetTrail
  , offsetTrail'
  , offsetPath
  , offsetPath'

    -- * Expansions

  , ExpandOpts(..), expandJoin, expandMiterLimit, expandCap, expandEpsilon
  , expandTrail
  , expandTrail'
  , expandPath
  , expandPath'

  ) where

import           Control.Applicative
import           Control.Lens            hiding (at)
import qualified Data.Semigroup          as Sem
import           Data.Typeable
import           Prelude

import           Data.Maybe              (catMaybes)
import           Data.Monoid.Inf

import           Data.Default.Class

import           Geometry.Direction
import           Geometry.Located
import           Geometry.Parametric
import           Geometry.Path
import           Geometry.Points
import           Geometry.Segment
import           Geometry.Space
import           Geometry.Trace
import           Geometry.Trail
import           Geometry.Transform
import           Geometry.TwoD.Arc
import           Geometry.TwoD.Curvature
import           Geometry.TwoD.Path      ()
import           Geometry.TwoD.Types
import           Geometry.TwoD.Vector    hiding (e)
import           Linear.Metric
import           Linear.Vector

trailSegments :: Num n => Trail V2 n -> [Segment V2 n]
trailSegments = toListOf segments

-- | The shape should be placed at the endpoints of lines. 'Default' is
--   'LineCapButt'.
data LineCap
  = LineCapButt   -- ^ Lines end precisely at their endpoints.
  | LineCapRound  -- ^ Lines are capped with semicircles centered on
                  --   endpoints.
  | LineCapSquare -- ^ Lines are capped with a squares centered on
                  --   endpoints.
  deriving (Eq, Ord, Show, Typeable)

-- | 'LineCapButt'
instance Default LineCap where
  def = LineCapButt

-- | 'Last' semigroup structure.
instance Sem.Semigroup LineCap where
  _ <> b = b

-- | How should the join points between line segments be drawn? The
--   'Default' is 'LineJoineMiter'.
data LineJoin
  = LineJoinMiter    -- ^ Use a \"miter\" shape (whatever that is).
  | LineJoinRound    -- ^ Use rounded join points.
  | LineJoinBevel    -- ^ Use a \"bevel\" shape (whatever that is).  Are
                     --   these... carpentry terms?
  deriving (Eq, Ord, Show, Typeable)

-- | Last semigroup structure.
instance Sem.Semigroup LineJoin where
  _ <> b = b


-- | 'LineJoinMiter'
instance Default LineJoin where
  def = LineJoinMiter


unitPerp :: OrderedField n => V2 n -> V2 n
unitPerp = signorm . perp

perpAtParam :: OrderedField n => Segment V2 n -> n -> V2 n
perpAtParam (Linear a) _ = negated $ unitPerp a
perpAtParam cubic t      = negated $ unitPerp a
  where
    (Cubic a _ _) = snd $ splitAtParam cubic t

-- | Compute the offset of a segment.  Given a segment compute the offset
--   curve that is a fixed distance from the original curve.  For linear
--   segments nothing special happens, the same linear segment is returned
--   with a point that is offset by a perpendicular vector of the given offset
--   length.
--
--   Cubic segments require a search for a subdivision of cubic segments that
--   gives an approximation of the offset within the given epsilon factor
--   (the given epsilon factor is applied to the radius giving a concrete epsilon
--   value).
--   We must do this because the offset of a cubic is not a cubic itself (the
--   degree of the curve increases).  Cubics do, however, approach constant
--   curvature as we subdivide.  In light of this we scale the handles of
--   the offset cubic segment in proportion to the radius of curvature difference
--   between the original subsegment and the offset which will have a radius
--   increased by the offset parameter.
--
--   In the following example the blue lines are the original segments and
--   the alternating green and red lines are the resulting offset trail segments.
--
--   <<diagrams/src_Geometry_TwoD_Offset_cubicOffsetExample.svg#diagram=cubicOffsetExample&width=600>>
--
--   Note that when the original curve has a cusp, the offset curve forms a
--   radius around the cusp, and when there is a loop in the original curve,
--   there can be two cusps in the offset curve.
--

-- | Options for specifying line join and segment epsilon for an offset
--   involving multiple segments.
data OffsetOpts d = OffsetOpts
  { _offsetJoin       :: LineJoin
  , _offsetMiterLimit :: d
  , _offsetEpsilon    :: d
  } deriving (Eq, Show)

makeLensesWith (lensRules & generateSignatures .~ False) ''OffsetOpts

-- | Specifies the style of join for between adjacent offset segments.
offsetJoin :: Lens' (OffsetOpts d) LineJoin

-- | Specifies the miter limit for the join.
offsetMiterLimit :: Lens' (OffsetOpts d) d

-- | Epsilon perimeter for 'offsetSegment'.
offsetEpsilon :: Lens' (OffsetOpts d) d

-- | The default offset options use the default 'LineJoin' ('LineJoinMiter'), a
--   miter limit of 10, and epsilon factor of 0.01.
instance Fractional d => Default (OffsetOpts d) where
  def = OffsetOpts def 10 0.01

-- | Options for specifying how a 'Trail' should be expanded.
data ExpandOpts d = ExpandOpts
  { _expandJoin       :: LineJoin
  , _expandMiterLimit :: d
  , _expandCap        :: LineCap
  , _expandEpsilon    :: d
  } deriving (Eq, Show)

makeLensesWith (lensRules & generateSignatures .~ False) ''ExpandOpts

-- | Specifies the style of join for between adjacent offset segments.
expandJoin :: Lens' (ExpandOpts d) LineJoin

-- | Specifies the miter limit for the join.
expandMiterLimit :: Lens' (ExpandOpts d) d

-- | Specifies how the ends are handled.
expandCap :: Lens' (ExpandOpts d) LineCap

-- | Epsilon perimeter for 'offsetSegment'.
expandEpsilon :: Lens' (ExpandOpts d) d


-- | The default 'ExpandOpts' is the default 'LineJoin' ('LineJoinMiter'),
--   miter limit of 10, default 'LineCap' ('LineCapButt'), and epsilon factor
--   of 0.01.
instance (Fractional d) => Default (ExpandOpts d) where
    def = ExpandOpts def 10 def 0.01

offsetSegment
  :: RealFloat n
  => n -- ^ Epsilon factor that when multiplied to the absolute value of
       --   the radius gives a value that represents the maximum allowed
       --   deviation from the true offset. In the current
       --   implementation each result segment should be bounded by arcs
       --   that are plus or minus epsilon factor from the radius of
       --   curvature of the offset.
  -> n -- ^ Offset from the original segment, positive is on the right
       --   of the curve, negative is on the left.
  -> Segment V2 n  -- ^ Original segment
  -> Located (Trail V2 n) -- ^ Resulting located (at the offset) trail.
offsetSegment _ r s@(Linear a) = fromSegments [s] `at` origin .+^ va
  where va = (-r) *^ unitPerp a

offsetSegment epsilon r s@(Cubic a b c) = t `at` origin .+^ va
  where
    t = fromSegments (go (radiusOfCurvature s 0.5))
    -- Perpendiculars to handles.
    va = (-r) *^ unitPerp a
    vc = (-r) *^ unitPerp (c ^-^ b)
    -- Split segments.
    ss = (\(x,y) -> [x,y]) $ splitAtParam s 0.5
    subdivided = concatMap (trailSegments . unLoc . offsetSegment epsilon r) ss

    -- Offset with handles scaled based on curvature.
    off factor = bezier3 (a^*factor) ((b ^-^ c)^*factor ^+^ c ^+^ vc ^-^ va) (c ^+^ vc ^-^ va)

    -- We observe a corner.  Subdivide right away.
    go (Finite 0) = subdivided
    -- We have some curvature
    go roc
      | close     = [o]
      | otherwise = subdivided
      where
        -- We want the multiplicative factor that takes us from the original
        -- segment's radius of curvature roc, to roc + r.
        --
        -- r + sr = x * sr
        --
        o = off $ case roc of
              Infinity  -> 1          -- Do the right thing.
              Finite sr -> 1 + r / sr

        close = and [epsilon * abs r > norm (p o ^+^ va ^-^ p s ^-^ pp s)
                    | t' <- [0.25, 0.5, 0.75]
                    , let p = (`atParam` t')
                    , let pp = (r *^) . (`perpAtParam` t')
                    ]


-- > {-# LANGUAGE TypeApplications #-}
-- > import Geometry.TwoD.Offset
-- >
-- > showExample :: Segment V2 Double -> Diagram V2
-- > showExample s = pad 1.1 . centerXY $ d # lc blue # lw thick <> d' # lw thick
-- >   where
-- >       d  = fromSegments [s]
-- >       d' = mconcat . zipWith lc colors . map (stroke @ _ @ (Path V2 Double)) . explodeTrail
-- >          $ offsetSegment 0.1 (-1) s
-- >
-- >       colors = cycle [green, red]
-- >
-- > cubicOffsetExample :: Diagram V2
-- > cubicOffsetExample = hcat . map showExample $
-- >         [ bezier3 (V2 10  0) (V2   5  18) (V2 10 20)
-- >         , bezier3 (V2  0 20) (V2  10  10) (V2  5 10)
-- >         , bezier3 (V2 10 20) (V2   0  10) (V2 10  0)
-- >         , bezier3 (V2 10 20) (V2 (-5) 10) (V2 10  0)
-- >         ]

-- Similar to (=<<).  This is when we want to map a function across something
-- located, but the result of the mapping will be transformable so we can
-- collapse the Located into the result.  This assumes that Located has the
-- meaning of merely taking something that cannot be translated and lifting
-- it into a space with translation.
bindLoc :: (InSpace v n a, HasBasis v, SameSpace a b, Transformable b) => (a -> b) -> Located a -> b
bindLoc f = join' . mapLoc f
  where
    join' (viewLoc -> (p,a)) = translate (p .-. origin) a

-- While we build offsets and expansions we will use the [Located (Segment Closed v)]
-- and [Located (Trail V2 n)] intermediate representations.
locatedTrailSegments
  :: OrderedField n
  => Located (Trail V2 n)
  -> [Located (Segment V2 n)]
locatedTrailSegments t = zipWith at (trailSegments (unLoc t)) (fromLocTrail t)

-- | Offset a 'Trail' with options and by a given radius.  This generates a new
--   trail that is always radius 'r' away from the given 'Trail' (depending on
--   the line join option) on the right.
--
--   The styles applied to an outside corner can be seen here (with the original
--   trail in blue and the result of 'offsetTrail'' in green):
--
--   <<diagrams/src_Geometry_TwoD_Offset_offsetTrailExample.svg#diagram=offsetTrailExample&width=600>>
--
--   When a negative radius is given, the offset trail will be on the left:
--
--   <<diagrams/src_Geometry_TwoD_Offset_offsetTrailLeftExample.svg#diagram=offsetTrailLeftExample&width=200>>
--
--   When offseting a counter-clockwise loop a positive radius gives an outer loop
--   while a negative radius gives an inner loop (both counter-clockwise).
--
--   <<diagrams/src_Geometry_TwoD_Offset_offsetTrailOuterExample.svg#diagram=offsetTrailOuterExample&width=300>>
--
offsetTrail'
  :: RealFloat n
  => OffsetOpts n
  -> n -- ^ Radius of offset. A negative value gives an offset on the
       --   left for a line and on the inside for a counter-clockwise
       --   loop.
  -> Located (Trail V2 n)
  -> Located (Trail V2 n)
offsetTrail' opts r t = joinSegments eps j isLoop (opts^.offsetMiterLimit) r ends . off $ t
    where
      eps = opts^.offsetEpsilon
      off = map (bindLoc (offsetSegment eps r)) . locatedTrailSegments
      ends | isLoop    = (\(a:as) -> as ++ [a]) . fromLocTrail $ t
           | otherwise = tail . fromLocTrail $ t
      j = fromLineJoin (opts^.offsetJoin)

      isLoop = withTrail (const False) (const True) (unLoc t)

-- | Offset a 'Trail' with the default options and a given radius. See
--   'offsetTrail''.
offsetTrail :: RealFloat n => n -> Located (Trail V2 n) -> Located (Trail V2 n)
offsetTrail = offsetTrail' def

-- | Offset a 'Path' by applying 'offsetTrail'' to each trail in the
--   path.
offsetPath' :: RealFloat n => OffsetOpts n -> n -> Path V2 n -> Path V2 n
offsetPath' opts r = mconcat
                   . fmap (bindLoc (fromLocTrail . offsetTrail' opts r) . (`at` origin))
                   . toListOf each

-- | Offset a 'Path' with the default options and given radius.  See 'offsetPath''.
offsetPath :: RealFloat n => n -> Path V2 n -> Path V2 n
offsetPath = offsetPath' def

-- TODO: Include arrowheads on examples to indicate direction so the "left" and
-- "right" make sense.
--
-- > import Geometry.TwoD.Offset
-- > import Data.Default.Class
-- > import Diagrams.TwoD.Text
-- >
-- > corner :: Located (Trail V2 Double)
-- > corner = fromVertices (map p2 [(0, 0), (10, 0), (5, 6)]) `at` origin
-- >
-- > offsetTrailExample :: Diagram V2
-- > offsetTrailExample = pad 1.1 . centerXY . lwO 3 . hsep 1
-- >                    . map (uncurry showStyle)
-- >                    $ [ (LineJoinMiter, "LineJoinMiter")
-- >                      , (LineJoinRound, "LineJoinRound")
-- >                      , (LineJoinBevel, "LineJoinBevel")
-- >                      ]
-- >  where
-- >    showStyle j s = centerXY (fromLocTrail corner # lc blue
-- >               <> fromLocTrail (offsetTrail' (def & offsetJoin .~ j) 2 corner) # lc green)
-- >            === (strutY 3 <> text s # font "Helvetica" # bold)
-- >
-- > offsetTrailLeftExample :: Diagram V2
-- > offsetTrailLeftExample = pad 1.1 . centerXY . lwO 3
-- >                        $ (fromLocTrail c # lc blue)
-- >                        <> (lc green . fromLocTrail
-- >                         . offsetTrail' (def & offsetJoin .~ LineJoinRound) (-2) $ c)
-- >   where
-- >     c = reflectY corner
-- >
-- > offsetTrailOuterExample :: Diagram V2
-- > offsetTrailOuterExample = pad 1.1 . centerXY . lwO 3
-- >                         $ (fromLocTrail c # lc blue)
-- >                         <> (lc green . fromLocTrail
-- >                          . offsetTrail' (def & offsetJoin .~ LineJoinRound) 2 $ c)
-- >   where
-- >     c = hexagon 5

withTrailL :: (Located (Line V2 n) -> r) -> (Located (Loop V2 n) -> r) -> Located (Trail V2 n) -> r
withTrailL f g l = withTrail (f . (`at` p)) (g . (`at` p)) (unLoc l)
  where
    p = loc l

-- | Expand a 'Trail' with the given options and radius 'r' around a given 'Trail'.
--   Expanding can be thought of as generating the loop that, when filled, represents
--   stroking the trail with a radius 'r' brush.
--
--   The cap styles applied to an outside corner can be seen here (with the original
--   trail in white and the result of 'expandTrail'' filled in green):
--
--   <<diagrams/src_Geometry_TwoD_Offset_expandTrailExample.svg#diagram=expandTrailExample&width=600>>
--
--   Loops result in a path with an inner and outer loop:
--
--   <<diagrams/src_Geometry_TwoD_Offset_expandLoopExample.svg#diagram=expandLoopExample&width=300>>
--
expandTrail' :: (OrderedField n, RealFloat n, RealFrac n)
             => ExpandOpts n
             -> n  -- ^ Radius of offset.  Only non-negative values allowed.
                        --   For a line this gives a loop of the offset.  For a
                        --   loop this gives two loops, the outer counter-clockwise
                        --   and the inner clockwise.
             -> Located (Trail V2 n)
             -> Path V2 n
expandTrail' o r t
  | r < 0     = error "expandTrail' with negative radius"
                -- TODO: consider just reversing the path instead of this error.
  | otherwise = withTrailL (toPath . expandLine o r) (expandLoop o r) t

expandLine :: RealFloat n => ExpandOpts n -> n -> Located (Line V2 n) -> Located (Trail V2 n)
expandLine opts r (mapLoc wrapLine -> t) = caps cap r s e (f r) (f $ -r)
    where
      eps = opts^.expandEpsilon
      off r' = map (bindLoc (offsetSegment eps r')) . locatedTrailSegments
      f r' = joinSegments eps (fromLineJoin (opts^.expandJoin)) False (opts^.expandMiterLimit) r' ends
           . off r' $ t
      ends = tail . fromLocTrail $ t
      s = atStart t
      e = atEnd t
      cap = fromLineCap (opts^.expandCap)

expandLoop :: RealFloat n => ExpandOpts n -> n -> Located (Loop V2 n) -> Path V2 n
expandLoop opts r (mapLoc wrapLoop -> t) =
  fromLocTrail (f r) Sem.<> (fromLocTrail . reversing . f $ -r)
    where
      eps = opts^.expandEpsilon
      off r' = map (bindLoc (offsetSegment eps r')) . locatedTrailSegments
      f r' = joinSegments eps (fromLineJoin (opts^.expandJoin)) True (opts^.expandMiterLimit) r' ends
           . off r' $ t
      ends = (\(a:as) -> as ++ [a]) . fromLocTrail $ t

-- | Expand a 'Trail' with the given radius and default options.  See 'expandTrail''.
expandTrail :: RealFloat n => n -> Located (Trail V2 n) -> Path V2 n
expandTrail = expandTrail' def

-- | Expand a 'Path' using 'expandTrail'' on each trail in the path.
expandPath' :: RealFloat n => ExpandOpts n -> n -> Path V2 n -> Path V2 n
expandPath' opts r = mconcat
                   . fmap (bindLoc (expandTrail' opts r) . (`at` origin))
                   . toListOf each

-- | Expand a 'Path' with the given radius and default options.  See 'expandPath''.
expandPath :: RealFloat n => n -> Path V2 n -> Path V2 n
expandPath = expandPath' def

-- > import Geometry.TwoD.Offset
-- > import Data.Default.Class
-- > import Diagrams.TwoD.Text
-- >
-- > corner :: Located (Trail V2 Double)
-- > corner = fromVertices (map p2 [(0, 0), (10, 0), (5, 6)]) `at` origin
-- >
-- > expandTrailExample :: Diagram V2
-- > expandTrailExample = pad 1.1 . centerXY . hsep 1
-- >                    . map (uncurry showStyle)
-- >                    $ [ (LineCapButt,   "LineCapButt")
-- >                      , (LineCapRound,  "LineCapRound")
-- >                      , (LineCapSquare, "LineCapSquare")
-- >                      ]
-- >  where
-- >    showStyle c s = centerXY (fromLocTrail corner # lc white # lw veryThick
-- >                               <> stroke (expandTrail'
-- >                                              (def & expandJoin .~ LineJoinRound
-- >                                                   & expandCap .~ c
-- >                                                   ) 2 corner)
-- >                                      # lw none # fc green)
-- >               === (strutY 3 <> text s # font "Helvetica" # bold)
-- >
-- > expandLoopExample :: Diagram V2
-- > expandLoopExample = pad 1.1 . centerXY $ ((stroke t # lw veryThick # lc white)
-- >                                        <> (stroke t' # lw none # fc green))
-- >   where
-- >     t  = mapLoc glueTrail $ fromVertices (map p2 [(0, 0), (5, 0), (10, 5), (10, 10), (0, 0)])
-- >     t' = expandTrail' (def & expandJoin .~ LineJoinRound) 1 t


-- | When we expand a line (the original line runs through the center of offset
--   lines at  r  and  -r) there is some choice in what the ends will look like.
--   If we are using a circle brush we should see a half circle at each end.
--   Similar caps could be made for square brushes or simply stopping exactly at
--   the end with a straight line (a perpendicular line brush).
--
--   caps  takes the radius and the start and end points of the original line and
--   the offset trails going out and coming back.  The result is a new list of
--   trails with the caps included.
caps :: RealFloat n => (n -> Point V2 n -> Point V2 n -> Point V2 n -> Trail V2 n)
     -> n -> Point V2 n -> Point V2 n -> Located (Trail V2 n) -> Located (Trail V2 n) -> Located (Trail V2 n)
caps cap r s e fs bs = mapLoc glueTrail $ mconcat
    [ cap r s (atStart bs) (atStart fs)
    , unLoc fs
    , cap r e (atEnd fs) (atEnd bs)
    , reversing (unLoc bs)
    ] `at` atStart bs

-- | Take a LineCap style and give a function for building the cap from
fromLineCap :: RealFloat n => LineCap -> n -> Point V2 n -> Point V2 n -> Point V2 n -> Trail V2 n
fromLineCap c = case c of
    LineCapButt   -> capCut
    LineCapRound  -> capArc
    LineCapSquare -> capSquare

-- | Builds a cap that directly connects the ends.
capCut :: RealFloat n => n -> Point V2 n -> Point V2 n -> Point V2 n -> Trail V2 n
capCut _r _c a b = fromSegments [straight (b .-. a)]

-- | Builds a cap with a square centered on the end.
capSquare :: RealFloat n => n -> Point V2 n -> Point V2 n -> Point V2 n -> Trail V2 n
capSquare _r c a b = unLoc $ fromVertices [ a, a .+^ v, b .+^ v, b ]
  where
    v = perp (a .-. c)

-- | Builds an arc to fit with a given radius, center, start, and end points.
--   A Negative r means a counter-clockwise arc
capArc :: RealFloat n => n -> Point V2 n -> Point V2 n -> Point V2 n -> Trail V2 n
capArc r c a b = fromLocTrail . moveTo c $ fs
  where
    fs | r < 0     = scale (-r) $ arcCW  (dirBetween a c) (dirBetween b c)
       | otherwise = scale r    $ arcCCW (dirBetween a c) (dirBetween b c)

-- | Join together a list of located trails with the given join style.  The
--   style is given as a function to compute the join given the local information
--   of the original vertex, the previous trail, and the next trail.  The result
--   is a single located trail.  A join radius is also given to aid in arc joins.
--
--   Note: this is not a general purpose join and assumes that we are joining an
--   offset trail.  For instance, a fixed radius arc will not fit between arbitrary
--   trails without trimming or extending.
joinSegments :: RealFloat n
             => n
             -> (n -> n -> Point V2 n -> Located (Trail V2 n) -> Located (Trail V2 n) -> Trail V2 n)
             -> Bool
             -> n
             -> n
             -> [Point V2 n]
             -> [Located (Trail V2 n)]
             -> Located (Trail V2 n)
joinSegments _ _ _ _ _ _ [] = mempty `at` origin
joinSegments _ _ _ _ _ [] _ = mempty `at` origin
joinSegments epsilon j isLoop ml r es ts@(t:_) = t'
  where
    t' | isLoop    = mapLoc (glueTrail . (Sem.<> f (take (length ts * 2 - 1) $ ss es (ts ++ [t])))) t
       | otherwise = mapLoc (Sem.<> f (ss es ts)) t
    ss es' ts' = concat [[test a b $ j ml r e a b, Just $ unLoc b] | (e,(a,b)) <- zip es' . (zip <*> tail) $ ts']
    test a b tj
        | atStart b `distance` atEnd a > epsilon = Just tj
        | otherwise                              = Nothing
    f = mconcat . catMaybes

-- | Take a join style and give the join function to be used by joinSegments.
fromLineJoin
  :: RealFloat n => LineJoin -> n -> n -> Point V2 n -> Located (Trail V2 n) -> Located (Trail V2 n) -> Trail V2 n
fromLineJoin j = case j of
    LineJoinMiter -> joinSegmentIntersect
    LineJoinRound -> joinSegmentArc
    LineJoinBevel -> joinSegmentClip

-- TODO: The joinSegmentCut option is not in our standard line joins.  I don't know
-- how useful it is graphically, I mostly had it as it was useful for debugging
{-
-- | Join with segments going back to the original corner.
joinSegmentCut :: (OrderedField n) => n -> n -> Point V2 n -> Located (Trail V2 n) -> Located (Trail V2 n) -> Trail V2 n
joinSegmentCut _ _ e a b = fromSegments
    [ straight (e .-. atEnd a)
    , straight (atStart b .-. e)
    ]
-}

-- | Join by directly connecting the end points.  On an inside corner this
--   creates negative space for even-odd fill.  Here is where we would want to
--   use an arc or something else in the future.
joinSegmentClip :: RealFloat n
  => n -> n -> Point V2 n -> Located (Trail V2 n) -> Located (Trail V2 n) -> Trail V2 n
joinSegmentClip _ _ _ a b = fromSegments [straight $ atStart b .-. atEnd a]

-- | Join with a radius arc.  On an inside corner this will loop around the interior
--   of the offset trail.  With a winding fill this will not be visible.
joinSegmentArc :: RealFloat n
  => n -> n -> Point V2 n -> Located (Trail V2 n) -> Located (Trail V2 n) -> Trail V2 n
joinSegmentArc _ r e a b = capArc r e (atEnd a) (atStart b)

-- | Join to the intersection of the incoming trails projected tangent to their ends.
--   If the intersection is beyond the miter limit times the radius, stop at the limit.
joinSegmentIntersect
    :: RealFloat n => n -> n -> Point V2 n -> Located (Trail V2 n) -> Located (Trail V2 n) -> Trail V2 n
joinSegmentIntersect miterLimit r e a b =
    if cross < 0.000001
      then clip
      else case traceP pa va t of
          -- clip join when we excede the miter limit.  We could instead
          -- Join at exactly the miter limit, but standard behavior seems
          -- to be clipping.
          Nothing -> clip
          Just p
            -- If trace gave us garbage...
            | p `distance` pb > abs (miterLimit * r) -> clip
            | otherwise                              -> unLoc $ fromVertices [ pa, p, pb ]
  where
    t = straight (miter vb) `at` pb
    va = unitPerp (pa .-. e)
    vb = negated $ unitPerp (pb .-. e)
    pa = atEnd a
    pb = atStart b
    miter v = abs (miterLimit * r) *^ v
    clip = joinSegmentClip miterLimit r e a b
    cross = let (xa,ya) = unr2 va; (xb,yb) = unr2 vb in abs (xa * yb - xb * ya)
