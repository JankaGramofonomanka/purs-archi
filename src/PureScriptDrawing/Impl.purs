module PureScriptDrawing.Impl where

import Prelude

import Effect (Effect)
import Data.Functor (class Functor)
import Control.Applicative (class Applicative)
import Control.Monad (class Monad)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Color as DrawingColor
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Number (sqrt)
import Data.Foldable (foldMap)
import Effect.Exception (throw)
import Geometry (BasicColor(..), Color(..), Fill(..), LineStyle(..), Opaqueness(..), Point(..), PointStyle(..), Thickness(..))
import Geometry as Geometry
import Graphics.Canvas as Canvas
import Graphics.Drawing as Drawing

newtype Renderer a = Renderer (Effect a)

runRenderer :: forall a . Renderer a -> Effect a
runRenderer (Renderer e) = e

derive newtype instance Functor Renderer
derive newtype instance Apply Renderer
derive newtype instance Bind Renderer
derive newtype instance Applicative Renderer
derive newtype instance Monad Renderer

instance geometryRenderer :: Geometry.Renderer Renderer where
  drawPolygon points (Fill { opaqueness: Opaqueness opacity, color }) = Renderer do
    canvas <- getCanvas
    context <- Canvas.getContext2D canvas
    Canvas.setGlobalAlpha context opacity
    Drawing.render context
      (Drawing.filled (Drawing.fillColor (toDrawingColor color))
        (Drawing.closed (map pointCoordinates points)))
    Canvas.setGlobalAlpha context 1.0

  drawLine start end thickness opacity style color = Renderer do
    canvas <- getCanvas
    dimensions <- Canvas.getCanvasDimensions canvas
    let (Point from) = start
    let (Point toward) = end
    let dx = toward.x - from.x
    let dy = toward.y - from.y
    let distance = sqrt (dx * dx + dy * dy)
    let extension =
          if distance == 0.0 then 0.0
          else 2.0 * (dimensions.width + dimensions.height) / distance
    drawSegment canvas
      { x: from.x - dx * extension, y: from.y - dy * extension }
      { x: from.x + dx * extension, y: from.y + dy * extension }
      thickness opacity style color

  drawLineSegment (Point from) (Point to) thickness opacity style color = Renderer do
    canvas <- getCanvas
    drawSegment canvas from to thickness opacity style color

  drawPoint (Point p) pointStyle color = Renderer do
    canvas <- getCanvas
    context <- Canvas.getContext2D canvas
    let drawing = case pointStyle of
          Circle -> Drawing.filled (Drawing.fillColor (toDrawingColor color))
            (Drawing.circle p.x p.y 4.0)
          Square -> Drawing.filled (Drawing.fillColor (toDrawingColor color))
            (Drawing.rectangle (p.x - 4.0) (p.y - 4.0) 8.0 8.0)
          Cross -> outlinedSegments
            [ [ { x: p.x - 4.0, y: p.y }, { x: p.x + 4.0, y: p.y } ]
            , [ { x: p.x, y: p.y - 4.0 }, { x: p.x, y: p.y + 4.0 } ]
            ]
          X -> outlinedSegments
            [ [ { x: p.x - 4.0, y: p.y - 4.0 }, { x: p.x + 4.0, y: p.y + 4.0 } ]
            , [ { x: p.x - 4.0, y: p.y + 4.0 }, { x: p.x + 4.0, y: p.y - 4.0 } ]
            ]
    Drawing.render context drawing
    where
    outlinedSegments = foldMap \points ->
      Drawing.outlined
        (Drawing.outlineColor (toDrawingColor color) <> Drawing.lineWidth 2.0)
        (Drawing.path points)

pointCoordinates :: Point -> { x :: Number, y :: Number }
pointCoordinates (Point p) = p

drawSegment
  :: Canvas.CanvasElement
  -> { x :: Number, y :: Number }
  -> { x :: Number, y :: Number }
  -> Thickness
  -> Opaqueness
  -> LineStyle
  -> Color
  -> Effect Unit
drawSegment canvas from to (Thickness width) (Opaqueness opacity) style color = do
  context <- Canvas.getContext2D canvas
  Canvas.setGlobalAlpha context opacity
  Canvas.setLineDash context (dashPattern style)
  Canvas.setLineCap context (capStyle style)
  Drawing.render context
    (Drawing.outlined
      (Drawing.outlineColor (toDrawingColor color) <> Drawing.lineWidth width)
      (Drawing.path [ from, to ])
    )
  Canvas.setGlobalAlpha context 1.0
  Canvas.setLineDash context []
  Canvas.setLineCap context Canvas.Butt

getCanvas :: Effect Canvas.CanvasElement
getCanvas = do
  maybeCanvas <- Canvas.getCanvasElementById "drawing-target"
  case maybeCanvas of
    Nothing -> throw "Missing #drawing-target canvas"
    Just canvas -> pure canvas

toDrawingColor :: Color -> DrawingColor.Color
toDrawingColor (RGB rgb) =
  DrawingColor.rgb (round rgb.r) (round rgb.g) (round rgb.b)
toDrawingColor (Basic basic) = case basic of
  Black -> DrawingColor.black
  White -> DrawingColor.white
  Red -> DrawingColor.rgb 255 0 0
  Green -> DrawingColor.rgb 0 128 0
  Blue -> DrawingColor.rgb 0 0 255
  Yellow -> DrawingColor.rgb 255 255 0
  Cyan -> DrawingColor.rgb 0 255 255
  Mag -> DrawingColor.rgb 255 0 255

dashPattern :: LineStyle -> Array Number
dashPattern Solid = []
dashPattern Dashed = [ 16.0, 12.0 ]
dashPattern Dotted = [ 0.0, 12.0 ]
dashPattern DashDot = [ 16.0, 8.0, 0.0, 8.0 ]

capStyle :: LineStyle -> Canvas.LineCap
capStyle Dotted = Canvas.Round
capStyle DashDot = Canvas.Round
capStyle _ = Canvas.Butt
