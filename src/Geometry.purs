module Geometry where

import Data.Unit (Unit)

data Point = Point { x :: Number, y :: Number }
point :: Number -> Number -> Point
point x y = Point { x, y }

data Color = RGB { r :: Number, g :: Number, b :: Number } | Basic BasicColor
data BasicColor = Black | White | Red | Green | Blue | Yellow | Cyan | Mag
newtype Thickness = Thickness Number
newtype Opaqueness = Opaqueness Number
data LineStyle = Solid | Dashed | Dotted | DashDot -- | DashDotDot
data PointStyle = Circle | Square | Cross | X

class Renderer m where
  drawRectangle :: Point -> Number -> Number-> Color -> m Unit
  drawLine :: Point -> Point -> Thickness -> Opaqueness -> LineStyle -> Color -> m Unit
  drawLineSegment :: Point -> Point -> Thickness -> Opaqueness -> LineStyle -> Color -> m Unit
  drawPoint :: Point -> PointStyle -> Color -> m Unit

drawLineHorizontal :: forall m. Renderer m => Number -> Thickness -> Opaqueness -> LineStyle -> Color -> m Unit
drawLineHorizontal y = drawLineSegment (point 0.0 y) (point 100.0 y)
drawLineVertical :: forall m. Renderer m => Number -> Thickness -> Opaqueness -> LineStyle -> Color -> m Unit
drawLineVertical x = drawLineSegment (point x 0.0) (point x 100.0)

drawLineSegmentHorizontal :: forall m. Renderer m => Number -> Number -> Number -> Thickness -> Opaqueness -> LineStyle -> Color -> m Unit
drawLineSegmentHorizontal y x1 x2 = drawLineSegment (point x1 y) (point x2 y)
drawLineSegmentVertical :: forall m. Renderer m => Number -> Number -> Number -> Thickness -> Opaqueness -> LineStyle -> Color -> m Unit
drawLineSegmentVertical x y1 y2 = drawLineSegment (point x y1) (point x y2)

