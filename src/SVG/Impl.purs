module SVG.Impl where

import Prelude

import Effect (Effect)
import Data.Functor (class Functor)
import Control.Applicative (class Applicative)
import Control.Monad (class Monad)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Data.Maybe (Maybe(..))
import Data.Number (sqrt)
import Geometry (BasicColor(..), Color(..), Contour(..), Fill(..), LineStyle(..), Opaqueness(..), Point(..), PointStyle(..), Thickness(..))
import Geometry as Geometry

newtype Renderer a = Renderer (Effect a)

runRenderer :: forall a . Renderer a -> Effect a
runRenderer (Renderer e) = e

derive newtype instance Functor Renderer
derive newtype instance Apply Renderer
derive newtype instance Bind Renderer
derive newtype instance Applicative Renderer
derive newtype instance Monad Renderer

instance geometryRenderer :: Geometry.Renderer Renderer where
  drawCircle (Point p) radius contour fill =
    let options = polygonOptions fill contour
    in Renderer (appendStyledCircle
      p.x p.y radius
      options.hasFill options.fillOpacity options.fillColor
      options.hasContour options.strokeWidth options.strokeOpacity
      options.strokeDash options.strokeCap options.strokeColor)

  drawArch (Point p) radius startAngle endAngle
    (Contour { thickness: Thickness width, opaqueness: Opaqueness opacity, lineStyle, color }) =
      Renderer (appendArc p.x p.y radius startAngle endAngle width opacity
        (dashArray lineStyle) (lineCap lineStyle) (colorCss color))

  drawCircleSegment (Point p) radius startAngle endAngle contour fill =
    let options = polygonOptions fill contour
    in Renderer (appendCircleSegment
      p.x p.y radius startAngle endAngle options.hasFill options.fillOpacity
      options.fillColor options.hasContour options.strokeWidth
      options.strokeOpacity options.strokeDash options.strokeCap options.strokeColor)

  drawPolygon points contour fill =
    let coordinates = map pointCoordinates points
        options = polygonOptions fill contour
    in Renderer (appendPolygon coordinates
      options.hasFill options.fillOpacity options.fillColor
      options.hasContour options.strokeWidth options.strokeOpacity
      options.strokeDash options.strokeCap options.strokeColor)

  drawLine (Point start) (Point end) (Thickness width) (Opaqueness opacity) style color =
    let dx = end.x - start.x
        dy = end.y - start.y
        length = sqrt (dx * dx + dy * dy)
        extension = if length == 0.0 then 0.0 else 1000000.0 / length
    in Renderer
      (appendLine
        (start.x - dx * extension)
        (start.y - dy * extension)
        (start.x + dx * extension)
        (start.y + dy * extension)
        width opacity (dashArray style) (lineCap style) (colorCss color))

  drawLineSegment (Point start) (Point end) (Thickness width) (Opaqueness opacity) style color =
    Renderer (appendLine start.x start.y end.x end.y width opacity (dashArray style) (lineCap style) (colorCss color))

  drawPoint (Point p) style color =
    Renderer case style of
      Circle -> appendCircle p.x p.y 4.0 (colorCss color)
      Square -> appendRectangle (p.x - 4.0) (p.y - 4.0) 8.0 8.0 1.0 (colorCss color)
      Cross -> appendPath (crossPath p.x p.y) (colorCss color) 2.0
      X -> appendPath (xPath p.x p.y) (colorCss color) 2.0

pointCoordinates :: Point -> { x :: Number, y :: Number }
pointCoordinates (Point p) = p

polygonOptions
  :: Maybe Fill
  -> Maybe Contour
  -> { hasFill :: Boolean
     , fillOpacity :: Number
     , fillColor :: String
     , hasContour :: Boolean
     , strokeWidth :: Number
     , strokeOpacity :: Number
     , strokeDash :: String
     , strokeCap :: String
     , strokeColor :: String
     }
polygonOptions fill contour =
  let fillOptions = case fill of
        Nothing -> { hasFill: false, fillOpacity: 1.0, fillColor: "" }
        Just (Fill { opaqueness: Opaqueness opacity, color }) ->
          { hasFill: true, fillOpacity: opacity, fillColor: colorCss color }
      contourOptions = case contour of
        Nothing ->
          { hasContour: false
          , strokeWidth: 0.0
          , strokeOpacity: 1.0
          , strokeDash: ""
          , strokeCap: "butt"
          , strokeColor: ""
          }
        Just (Contour { thickness: Thickness width, opaqueness: Opaqueness opacity, lineStyle, color }) ->
          { hasContour: true
          , strokeWidth: width
          , strokeOpacity: opacity
          , strokeDash: dashArray lineStyle
          , strokeCap: lineCap lineStyle
          , strokeColor: colorCss color
          }
  in
    { hasFill: fillOptions.hasFill
    , fillOpacity: fillOptions.fillOpacity
    , fillColor: fillOptions.fillColor
    , hasContour: contourOptions.hasContour
    , strokeWidth: contourOptions.strokeWidth
    , strokeOpacity: contourOptions.strokeOpacity
    , strokeDash: contourOptions.strokeDash
    , strokeCap: contourOptions.strokeCap
    , strokeColor: contourOptions.strokeColor
    }

colorCss :: Color -> String
colorCss (RGB rgb) =
  "rgb(" <> show rgb.r <> "," <> show rgb.g <> "," <> show rgb.b <> ")"
colorCss (Basic basic) = case basic of
  Black -> "black"
  White -> "white"
  Red -> "red"
  Green -> "green"
  Blue -> "blue"
  Yellow -> "yellow"
  Cyan -> "cyan"
  Mag -> "magenta"

dashArray :: LineStyle -> String
dashArray Solid = ""
dashArray Dashed = "16 12"
dashArray Dotted = "0 12"
dashArray DashDot = "16 8 0 8"

lineCap :: LineStyle -> String
lineCap Dotted = "round"
lineCap DashDot = "round"
lineCap _ = "butt"

crossPath :: Number -> Number -> String
crossPath x y =
  "M " <> show (x - 4.0) <> " " <> show y
    <> " L " <> show (x + 4.0) <> " " <> show y
    <> " M " <> show x <> " " <> show (y - 4.0)
    <> " L " <> show x <> " " <> show (y + 4.0)

xPath :: Number -> Number -> String
xPath x y =
  "M " <> show (x - 4.0) <> " " <> show (y - 4.0)
    <> " L " <> show (x + 4.0) <> " " <> show (y + 4.0)
    <> " M " <> show (x - 4.0) <> " " <> show (y + 4.0)
    <> " L " <> show (x + 4.0) <> " " <> show (y - 4.0)

foreign import appendRectangle :: Number -> Number -> Number -> Number -> Number -> String -> Effect Unit
foreign import appendPolygon
  :: Array { x :: Number, y :: Number }
  -> Boolean
  -> Number
  -> String
  -> Boolean
  -> Number
  -> Number
  -> String
  -> String
  -> String
  -> Effect Unit
foreign import appendStyledCircle
  :: Number
  -> Number
  -> Number
  -> Boolean
  -> Number
  -> String
  -> Boolean
  -> Number
  -> Number
  -> String
  -> String
  -> String
  -> Effect Unit
foreign import appendArc
  :: Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> String
  -> String
  -> String
  -> Effect Unit
foreign import appendCircleSegment
  :: Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Boolean
  -> Number
  -> String
  -> Boolean
  -> Number
  -> Number
  -> String
  -> String
  -> String
  -> Effect Unit
foreign import appendLine :: Number -> Number -> Number -> Number -> Number -> Number -> String -> String -> String -> Effect Unit
foreign import appendCircle :: Number -> Number -> Number -> String -> Effect Unit
foreign import appendPath :: String -> String -> Number -> Effect Unit
foreign import prepareSvg :: Effect { width :: Number, height :: Number }
