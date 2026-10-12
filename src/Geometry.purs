module Geometry where

import Prelude
import Data.Maybe (Maybe)

data Point = Point { x :: Number, y :: Number }
point :: Number -> Number -> Point
point x y = Point { x, y }

data Color = RGB { r :: Number, g :: Number, b :: Number } | Basic BasicColor
data BasicColor = Black | White | Red | Green | Blue | Yellow | Cyan | Mag
newtype Thickness = Thickness Number
newtype Opaqueness = Opaqueness Number
data LineStyle = Solid | Dashed | Dotted | DashDot -- | DashDotDot
data PointStyle = Circle | Square | Cross | X

data Contour = Contour
  { thickness :: Thickness
  , opaqueness :: Opaqueness
  , lineStyle :: LineStyle
  , color :: Color
  }

data Fill = Fill
  { opaqueness :: Opaqueness
  , color :: Color
  }

class Renderer m where
  drawLine :: Point -> Point -> Contour -> m Unit
  drawLineSegment :: Point -> Point -> Contour -> m Unit
  drawPoint :: Point -> PointStyle -> Color -> m Unit
  drawPolygon :: Array Point -> Maybe Contour -> Maybe Fill -> m Unit
  drawCircle :: Point -> Number -> Maybe Contour -> Maybe Fill -> m Unit
  drawArch :: Point -> Number -> Number -> Number -> Contour -> m Unit
  drawCircleSegment :: Point -> Number -> Number -> Number -> Maybe Contour -> Maybe Fill -> m Unit

drawRectangle
  :: forall m
   . Renderer m
  => Point
  -> Number
  -> Number
  -> Maybe Contour
  -> Maybe Fill
  -> m Unit
drawRectangle (Point p) width height contour fill =
  drawPolygon
    [ point p.x p.y
    , point (p.x + width) p.y
    , point (p.x + width) (p.y + height)
    , point p.x (p.y + height)
    ]
    contour fill

drawTriangle
  :: forall m
   . Renderer m
  => Point
  -> Point
  -> Point
  -> Maybe Contour
  -> Maybe Fill
  -> m Unit
drawTriangle first second third contour fill =
  drawPolygon [ first, second, third ] contour fill

drawLineHorizontal :: forall m. Renderer m => Number -> Contour -> m Unit
drawLineHorizontal y = drawLineSegment (point 0.0 y) (point 100.0 y)
drawLineVertical :: forall m. Renderer m => Number -> Contour -> m Unit
drawLineVertical x = drawLineSegment (point x 0.0) (point x 100.0)

drawLineSegmentHorizontal :: forall m. Renderer m => Number -> Number -> Number -> Contour -> m Unit
drawLineSegmentHorizontal y x1 x2 = drawLineSegment (point x1 y) (point x2 y)
drawLineSegmentVertical :: forall m. Renderer m => Number -> Number -> Number -> Contour -> m Unit
drawLineSegmentVertical x y1 y2 = drawLineSegment (point x y1) (point x y2)
