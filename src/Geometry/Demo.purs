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

draw :: forall m. Monad m => Geometry.Renderer m => Number -> Number -> m Unit
draw width height = do
  let centerX = width / 2.0
  let centerY = height / 2.0
  Geometry.drawRectangle (point (centerX - 100.0) (centerY - 50.0)) 100.0 100.0 Nothing (Just $ Geometry.Fill { opaqueness: opaque, color: blue })
  Geometry.drawRectangle (point centerX (centerY - 50.0)) 100.0 100.0 Nothing (Just $ Geometry.Fill { opaqueness: opaque, color: red })
  Geometry.drawTriangle
    (point (centerX - 100.0) (centerY + 50.0))
    (point (centerX + 100.0) (centerY + 50.0))
    (point centerX (centerY + 200.0))
    Nothing
    (Just $ Geometry.Fill { opaqueness: opaque, color: purple })
  Geometry.drawRectangle (point (centerX - 50.0) (centerY)) 100.0 100.0 Nothing (Just $ Geometry.Fill { opaqueness: Opaqueness 0.5, color: teal })
  Geometry.drawPolygon
    [ point (centerX + 100.0) (centerY + 50.0)
    , point centerX (centerY + 200.0)
    , point (centerX + 100.0) (centerY + 350.0)
    , point (centerX + 200.0) (centerY + 200.0)
    , point (centerX + 300.0) (centerY + 200.0)
    , point (centerX + 200.0) (centerY + 50.0)
    ]
    (Just $ Geometry.Contour { thickness: Thickness 2.0, opaqueness: opaque, lineStyle: Solid, color: ink })
    Nothing
  Geometry.drawCircle (point centerX (centerY - 50.0)) 50.0 Nothing (Just $ Geometry.Fill { opaqueness: Opaqueness 0.5, color: orange })
  Geometry.drawCircleSegment (point centerX (centerY - 150.0)) 50.0 pi (2.0 * pi) (Just $ Geometry.Contour { thickness: Thickness 4.0, opaqueness: opaque, lineStyle: DashDot, color: ink }) (Just $ Geometry.Fill { opaqueness: opaque, color: teal })

  Geometry.drawLine (point 0.0 (centerY - 350.0)) (point width (centerY - 350.0))
    (contour (Thickness 4.0) opaque Dashed orange)
  Geometry.drawLine (point 0.0 (centerY - 250.0)) (point width (centerY - 250.0))
    (contour (Thickness 4.0) opaque Dotted teal)
  Geometry.drawLine (point 0.0 (centerY - 150.0)) (point width (centerY - 150.0))
    (contour (Thickness 4.0) opaque DashDot purple)
  Geometry.drawLine (point 0.0 (centerY - 50.0)) (point width (centerY - 50.0))
    (contour (Thickness 4.0) opaque Solid ink)
  Geometry.drawLineSegment (point (centerX + 100.0) (centerY - 150.0))
    (point (centerX + 100.0) (centerY + 150.0))
    (contour (Thickness 4.0) opaque Solid ink)
  Geometry.drawLineSegment (point (centerX + 200.0) (centerY - 150.0))
    (point (centerX + 200.0) (centerY + 150.0))
    (contour (Thickness 6.0) (Opaqueness 0.1) Solid ink)
  Geometry.drawLineSegment (point (centerX + 300.0) (centerY - 150.0))
    (point (centerX + 300.0) (centerY + 150.0))
    (contour (Thickness 4.0) (Opaqueness 0.3) Solid ink)
  Geometry.drawLineSegment (point (centerX + 400.0) (centerY - 150.0))
    (point (centerX + 400.0) (centerY + 150.0))
    (contour (Thickness 2.0) opaque Solid ink)
  Geometry.drawArch (point centerX (centerY - 100.0)) 50.0 pi (2.0 * pi) (Geometry.Contour { thickness: Thickness 4.0, opaqueness: opaque, lineStyle: Solid, color: ink })

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
