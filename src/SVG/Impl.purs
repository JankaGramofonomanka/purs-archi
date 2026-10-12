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
  drawPolygon points contour (Fill { opaqueness: Opaqueness opacity, color }) =
    let coordinates = map pointCoordinates points
        fill = colorCss color
    in Renderer case contour of
      Nothing ->
        appendPolygon coordinates opacity fill false 0.0 1.0 "" "butt" ""
      Just (Contour { thickness: Thickness width, opaqueness: Opaqueness contourOpacity, lineStyle, color: contourColor }) ->
        appendPolygon coordinates opacity fill true width contourOpacity
          (dashArray lineStyle) (lineCap lineStyle) (colorCss contourColor)

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
