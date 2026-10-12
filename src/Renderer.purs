module Renderer where

import Prelude
import Data.Maybe (Maybe)
import Data.Default (class Default)
import Data.Newtype (class Newtype)

newtype Point = Point { x :: Number, y :: Number }
point :: Number -> Number -> Point
point x y = Point { x, y }

derive instance Newtype Point _

data Color = RGB { r :: Number, g :: Number, b :: Number } | Basic BasicColor
data BasicColor = Black | White | Red | Green | Blue | Yellow | Cyan | Mag

newtype Thickness = Thickness Number
derive instance Newtype Thickness _
newtype Opaqueness = Opaqueness Number

derive instance Newtype Opaqueness _

data LineStyle = Solid | Dashed | Dotted | DashDot -- | DashDotDot
data PointStyle = Circle | Square | Cross | X

newtype Contour = Contour
  { thickness :: Thickness
  , opaqueness :: Opaqueness
  , lineStyle :: LineStyle
  , color :: Color
  }

derive instance Newtype Contour _

instance Default Contour where
  def = Contour { thickness: Thickness 4.0, opaqueness: Opaqueness 1.0, lineStyle: Solid, color: Basic Black }

newtype Fill = Fill
  { opaqueness :: Opaqueness
  , color :: Color
  }

derive instance Newtype Fill _

instance Default Fill where
  def = Fill { opaqueness: Opaqueness 1.0, color: Basic White }

type LineRenderer m = Contour -> m Unit
type AreaRenderer m = Maybe Contour -> Maybe Fill -> m Unit

class Renderer m where
  drawLine :: Point -> Point -> LineRenderer m
  drawLineSegment :: Point -> Point -> LineRenderer m
  drawPoint :: Point -> PointStyle -> Color -> m Unit
  drawPolygon :: Array Point -> AreaRenderer m
  drawCircle :: Point -> Number -> AreaRenderer m
  drawArch :: Point -> Number -> Number -> Number -> LineRenderer m
  drawCircleSegment :: Point -> Number -> Number -> Number -> AreaRenderer m

drawRectangle
  :: forall m
   . Renderer m
  => Point
  -> Number
  -> Number
  -> AreaRenderer m
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
  -> AreaRenderer m
drawTriangle first second third contour fill =
  drawPolygon [ first, second, third ] contour fill

drawLineHorizontal :: forall m. Renderer m => Number -> LineRenderer m
drawLineHorizontal y = drawLineSegment (point 0.0 y) (point 100.0 y)
drawLineVertical :: forall m. Renderer m => Number -> LineRenderer m
drawLineVertical x = drawLineSegment (point x 0.0) (point x 100.0)

drawLineSegmentHorizontal :: forall m. Renderer m => Number -> Number -> Number -> LineRenderer m
drawLineSegmentHorizontal y x1 x2 = drawLineSegment (point x1 y) (point x2 y)
drawLineSegmentVertical :: forall m. Renderer m => Number -> Number -> Number -> LineRenderer m
drawLineSegmentVertical x y1 y2 = drawLineSegment (point x y1) (point x y2)
