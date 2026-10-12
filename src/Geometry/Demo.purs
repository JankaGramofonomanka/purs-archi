module Geometry.Demo (draw) where

import Prelude

import Data.Maybe (Maybe(..))
import Geometry
  ( Color(..)
  , LineStyle(..)
  , Opaqueness(..)
  , PointStyle(..)
  , Thickness(..)
  , point
  )
import Geometry as Geometry
import Data.Number (pi)
import Data.Default (class Default, def)
import Data.Newtype (class Newtype, modify)

draw :: forall m. Monad m => Geometry.Renderer m => Number -> Number -> m Unit
draw width height = do
  let centerX = width / 2.0
  let centerY = height / 2.0
  Geometry.drawRectangle (point (centerX - 100.0) (centerY - 50.0)) 100.0 100.0 Nothing (Just $ modify (_ { color = blue }) def)
  Geometry.drawRectangle (point centerX (centerY - 50.0)) 100.0 100.0 Nothing (Just $ modify (_ { color = red }) def)
  Geometry.drawTriangle
    (point (centerX - 100.0) (centerY + 50.0))
    (point (centerX + 100.0) (centerY + 50.0))
    (point centerX (centerY + 200.0))
    Nothing
    (Just $ modify (_ { color = purple }) def)
  Geometry.drawRectangle (point (centerX - 50.0) (centerY)) 100.0 100.0 Nothing (Just $ Geometry.Fill { opaqueness: Opaqueness 0.5, color: teal })
  Geometry.drawPolygon
    [ point (centerX + 100.0) (centerY + 50.0)
    , point centerX (centerY + 200.0)
    , point (centerX + 100.0) (centerY + 350.0)
    , point (centerX + 200.0) (centerY + 200.0)
    , point (centerX + 300.0) (centerY + 200.0)
    , point (centerX + 200.0) (centerY + 50.0)
    ]
    (Just $ modify (_ { thickness = Thickness 2.0 }) def)
    Nothing
  Geometry.drawCircleSegment (point centerX (centerY - 225.0)) 50.0 0.0 pi (Just $ modify (_ { lineStyle = DashDot }) def) (Just $ modify (_ { color = teal }) def)
  Geometry.drawCircle (point centerX (centerY - 150.0)) 50.0 Nothing (Just $ Geometry.Fill { opaqueness: Opaqueness 0.5, color: orange })

  Geometry.drawLine (point 0.0 (centerY - 350.0)) (point width (centerY - 350.0))
    (modify (_ { lineStyle = Dashed, color = orange }) def)
  Geometry.drawLine (point 0.0 (centerY - 250.0)) (point width (centerY - 250.0))
    (modify (_ { lineStyle = Dotted, color = teal }) def)
  Geometry.drawLine (point 0.0 (centerY - 150.0)) (point width (centerY - 150.0))
    (modify (_ { lineStyle = DashDot, color = purple }) def)
  Geometry.drawLine (point 0.0 (centerY - 50.0)) (point width (centerY - 50.0)) def
  Geometry.drawLineSegment (point (centerX + 100.0) (centerY - 150.0)) (point (centerX + 100.0) (centerY + 150.0)) def
  Geometry.drawLineSegment (point (centerX + 200.0) (centerY - 150.0))
    (point (centerX + 200.0) (centerY + 150.0))
    (modify (_ { thickness = Thickness 6.0, opaqueness = Opaqueness 0.1 }) def)
  Geometry.drawLineSegment (point (centerX + 300.0) (centerY - 150.0))
    (point (centerX + 300.0) (centerY + 150.0))
    (modify (_ { opaqueness = Opaqueness 0.3 }) def)
  Geometry.drawLineSegment (point (centerX + 400.0) (centerY - 150.0))
    (point (centerX + 400.0) (centerY + 150.0))
    (modify (_ { thickness = Thickness 2.0 }) def)
  Geometry.drawArch (point centerX (centerY - 75.0)) 50.0 pi (2.0 * pi) def

  Geometry.drawPoint (point (centerX - 200.0) (centerY + 50.0)) Circle ink
  Geometry.drawPoint (point (centerX - 100.0) (centerY + 50.0)) Cross ink
  Geometry.drawPoint (point (centerX - 200.0) (centerY + 150.0)) Square ink
  Geometry.drawPoint (point (centerX - 100.0) (centerY + 150.0)) X ink


blue :: Color
blue = RGB { r: 82.0, g: 104.0, b: 216.0 }

red :: Color
red = RGB { r: 229.0, g: 57.0, b: 53.0 }

orange :: Color
orange = RGB { r: 194.0, g: 65.0, b: 12.0 }

teal :: Color
teal = RGB { r: 15.0, g: 118.0, b: 110.0 }

purple :: Color
purple = RGB { r: 126.0, g: 34.0, b: 206.0 }

ink :: Color
ink = RGB { r: 28.0, g: 37.0, b: 64.0 }

opaque :: Opaqueness
opaque = Opaqueness 1.0

contour :: Thickness -> Opaqueness -> LineStyle -> Color -> Geometry.Contour
contour thickness opaqueness lineStyle color =
  Geometry.Contour { thickness, opaqueness, lineStyle, color }
