module Renderer.Demo (draw) where

import Prelude

import Architecture (corynthian, doric, ionic, minuteFromColumnAndEntablatureHeight, minuteFromLowerDiameter, minuteFromUpperDiameter, orderHeight, sizeOf)
import Architecture.Draw (drawOrder, drawOrderFromLowerDiameter, drawOrderFromOrderHeight, drawOrderFromUpperDiameter)
import Data.Default (class Default, def)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, modify)
import Data.Number (pi)
import Geometry (Rectangle(..))
import Ortogonal as Ortogonal
import Renderer (Color(..), LineStyle(..), Opaqueness(..), PointStyle(..), Thickness(..), point)
import Renderer as Renderer

draw :: forall m. Monad m => Renderer.Renderer m => Number -> Number -> m Unit
draw width height = do
  let screenCenterX = width / 2.0
  let screenCenterY = height / 2.0

  let centerX = screenCenterX
  let centerY = screenCenterY
  Renderer.drawRectangle (point (centerX - 100.0) (centerY - 50.0)) 100.0 100.0 Nothing (Just $ modify (_ { color = blue }) def)
  Renderer.drawRectangle (point centerX (centerY - 50.0)) 100.0 100.0 Nothing (Just $ modify (_ { color = red }) def)
  Renderer.drawTriangle
    (point (centerX - 100.0) (centerY + 50.0))
    (point (centerX + 100.0) (centerY + 50.0))
    (point centerX (centerY + 200.0))
    Nothing
    (Just $ modify (_ { color = purple }) def)
  Renderer.drawRectangle (point (centerX - 50.0) (centerY)) 100.0 100.0 Nothing (Just $ Renderer.Fill { opaqueness: Opaqueness 0.5, color: teal })
  Renderer.drawPolygon
    [ point (centerX + 100.0) (centerY + 50.0)
    , point centerX (centerY + 200.0)
    , point (centerX + 100.0) (centerY + 350.0)
    , point (centerX + 200.0) (centerY + 200.0)
    , point (centerX + 300.0) (centerY + 200.0)
    , point (centerX + 200.0) (centerY + 50.0)
    ]
    (Just $ modify (_ { thickness = Thickness 2.0 }) def)
    Nothing
  Renderer.drawCircleSegment (point centerX (centerY - 225.0)) 50.0 0.0 pi (Just $ modify (_ { lineStyle = DashDot }) def) (Just $ modify (_ { color = teal }) def)
  Renderer.drawCircle (point centerX (centerY - 150.0)) 50.0 Nothing (Just $ Renderer.Fill { opaqueness: Opaqueness 0.5, color: orange })

  Renderer.drawLine (point 0.0 (centerY - 350.0)) (point width (centerY - 350.0))
    (modify (_ { lineStyle = Dashed, color = orange }) def)
  Renderer.drawLine (point 0.0 (centerY - 250.0)) (point width (centerY - 250.0))
    (modify (_ { lineStyle = Dotted, color = teal }) def)
  Renderer.drawLine (point 0.0 (centerY - 150.0)) (point width (centerY - 150.0))
    (modify (_ { lineStyle = DashDot, color = purple }) def)
  Renderer.drawLineHorizontal (centerY - 50.0) def
  Renderer.drawLineVertical (centerX + 100.0) def
  Renderer.drawLineSegment (point (centerX + 200.0) (centerY - 150.0))
    (point (centerX + 200.0) (centerY + 150.0))
    (modify (_ { thickness = Thickness 6.0, opaqueness = Opaqueness 0.1 }) def)
  Renderer.drawLineSegment (point (centerX + 300.0) (centerY - 150.0))
    (point (centerX + 300.0) (centerY + 150.0))
    (modify (_ { opaqueness = Opaqueness 0.3 }) def)
  Renderer.drawLineSegment (point (centerX + 400.0) (centerY - 150.0))
    (point (centerX + 400.0) (centerY + 150.0))
    (modify (_ { thickness = Thickness 2.0 }) def)
  Renderer.drawArch (point centerX (centerY - 75.0)) 50.0 pi (2.0 * pi) def

  Renderer.drawPoint (point (centerX - 200.0) (centerY + 50.0)) Circle ink
  Renderer.drawPoint (point (centerX - 100.0) (centerY + 50.0)) Cross ink
  Renderer.drawPoint (point (centerX - 200.0) (centerY + 150.0)) Square ink
  Renderer.drawPoint (point (centerX - 100.0) (centerY + 150.0)) X ink

  -- render ortogonal
  let centerX = screenCenterY - 500.0
  let rectangle = Rectangle { width: 100.0, height: 200.0 }
  let left = centerX - 60.0
  let right = centerX + 70.0
  let top = centerY - 120.0
  let bottom = centerY + 140.0
  Ortogonal.drawCentered       (point centerX centerY) rectangle
  Ortogonal.drawLeftCentered   (point left    centerY) rectangle
  Ortogonal.drawRightCentered  (point right   centerY) rectangle
  Ortogonal.drawTopCentered    (point centerX top)     rectangle
  Ortogonal.drawBottomCentered (point centerX bottom)  rectangle
  Ortogonal.drawTopLeft        (point left    top)     rectangle
  Ortogonal.drawTopRight       (point right   top)     rectangle
  Ortogonal.drawBottomLeft     (point left    bottom)  rectangle
  Ortogonal.drawBottomRight    (point right   bottom)  rectangle

  --orders
  let centerX = screenCenterX + 500.0
  let ionicHeight = 200.0
  let ionicMinute = minuteFromColumnAndEntablatureHeight ionic ionicHeight
  let ionicBottom = screenCenterY + ionicHeight / 2.0
  let ionicLowerDiameter = sizeOf ionic.column.lowerDiameter ionicMinute
  let ionicUpperDiameter = sizeOf ionic.column.upperDiameter ionicMinute

  let doricUpperDiameter = ionicLowerDiameter
  let doricMinute = minuteFromUpperDiameter doric doricUpperDiameter
  let doricHeight = orderHeight doricMinute doric
  let doricBottom = ionicBottom + doricHeight

  let corynthianLowerDiameter = ionicUpperDiameter
  let corynthianMinute = minuteFromLowerDiameter corynthian corynthianLowerDiameter
  let corynthianBottom = screenCenterY - ionicHeight / 2.0

  drawOrderFromUpperDiameter (point centerX doricBottom) doricUpperDiameter doric
  drawOrderFromOrderHeight (point centerX ionicBottom) ionicHeight ionic
  drawOrderFromLowerDiameter (point centerX corynthianBottom) corynthianLowerDiameter corynthian

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

contour :: Thickness -> Opaqueness -> LineStyle -> Color -> Renderer.Contour
contour thickness opaqueness lineStyle color =
  Renderer.Contour { thickness, opaqueness, lineStyle, color }
