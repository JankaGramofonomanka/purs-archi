module Canvas.Impl where

import Prelude

import Effect (Effect)
import Data.Functor (class Functor)
import Control.Applicative (class Applicative)
import Control.Monad (class Monad)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Number (pi, sqrt)
import Effect.Exception (throw)
import Geometry (BasicColor(..), Color(..), Contour(..), Fill(..), LineStyle(..), Opaqueness(..), Point(..), PointStyle(..), Thickness(..))
import Geometry as Geometry
import Graphics.Canvas as Canvas

newtype Renderer a = Renderer (Effect a)

runRenderer :: forall a . Renderer a -> Effect a
runRenderer (Renderer e) = e

derive newtype instance Functor Renderer
derive newtype instance Apply Renderer
derive newtype instance Bind Renderer
derive newtype instance Applicative Renderer
derive newtype instance Monad Renderer

instance geometryRenderer :: Geometry.Renderer Renderer where
  drawCircle (Point p) radius contour fill = Renderer do
    context <- getContext
    Canvas.beginPath context
    Canvas.arc context
      { x: p.x, y: p.y, radius, start: 0.0, end: 2.0 * pi
      , useCounterClockwise: false
      }
    case fill of
      Nothing -> pure unit
      Just (Fill { opaqueness: Opaqueness opacity, color }) -> do
        Canvas.setFillStyle context (colorCss color)
        Canvas.setGlobalAlpha context opacity
        Canvas.fill context
        Canvas.setGlobalAlpha context 1.0
    case contour of
      Nothing -> pure unit
      Just (Contour { thickness: Thickness width, opaqueness: Opaqueness opacity, lineStyle, color }) -> do
        Canvas.setStrokeStyle context (colorCss color)
        Canvas.setLineWidth context width
        Canvas.setGlobalAlpha context opacity
        Canvas.setLineDash context (dashPattern lineStyle)
        Canvas.setLineCap context (capStyle lineStyle)
        Canvas.stroke context
        Canvas.setGlobalAlpha context 1.0
        Canvas.setLineDash context []
        Canvas.setLineCap context Canvas.Butt

  drawArch (Point p) radius startAngle endAngle (Contour { thickness: Thickness width, opaqueness: Opaqueness opacity, lineStyle, color }) = Renderer do
    context <- getContext
    Canvas.beginPath context
    Canvas.arc context
      { x: p.x, y: p.y, radius, start: startAngle, end: endAngle
      , useCounterClockwise: false
      }
    Canvas.setStrokeStyle context (colorCss color)
    Canvas.setLineWidth context width
    Canvas.setGlobalAlpha context opacity
    Canvas.setLineDash context (dashPattern lineStyle)
    Canvas.setLineCap context (capStyle lineStyle)
    Canvas.stroke context
    Canvas.setGlobalAlpha context 1.0
    Canvas.setLineDash context []
    Canvas.setLineCap context Canvas.Butt

  drawPolygon points contour fill = Renderer do
    if Array.length points < 3 then pure unit
    else case Array.uncons points of
      Nothing -> pure unit
      Just { head: Point first, tail } -> do
        context <- getContext
        Canvas.beginPath context
        Canvas.moveTo context first.x first.y
        traverse_ (\(Point p) -> Canvas.lineTo context p.x p.y) tail
        Canvas.closePath context
        case fill of
          Nothing -> pure unit
          Just (Fill { opaqueness: Opaqueness opacity, color }) -> do
            Canvas.setFillStyle context (colorCss color)
            Canvas.setGlobalAlpha context opacity
            Canvas.fill context
            Canvas.setGlobalAlpha context 1.0
        case contour of
          Nothing -> pure unit
          Just (Contour { thickness: Thickness width, opaqueness: Opaqueness contourOpacity, lineStyle, color: contourColor }) -> do
            Canvas.setStrokeStyle context (colorCss contourColor)
            Canvas.setLineWidth context width
            Canvas.setGlobalAlpha context contourOpacity
            Canvas.setLineDash context (dashPattern lineStyle)
            Canvas.setLineCap context (capStyle lineStyle)
            Canvas.stroke context
            Canvas.setGlobalAlpha context 1.0
            Canvas.setLineDash context []
            Canvas.setLineCap context Canvas.Butt

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
    drawSegment
      canvas
      { x: from.x - dx * extension, y: from.y - dy * extension }
      { x: from.x + dx * extension, y: from.y + dy * extension }
      thickness
      opacity
      style
      color

  drawLineSegment (Point from) (Point to) thickness opacity style color = Renderer do
    canvas <- getCanvas
    drawSegment canvas from to thickness opacity style color

  drawPoint (Point p) pointStyle color = Renderer do
    context <- getContext
    Canvas.setFillStyle context (colorCss color)
    Canvas.setStrokeStyle context (colorCss color)
    Canvas.setLineWidth context 2.0
    case pointStyle of
      Circle -> do
        Canvas.beginPath context
        Canvas.arc context
          { x: p.x, y: p.y, radius: 4.0, start: 0.0, end: 2.0 * pi
          , useCounterClockwise: false
          }
        Canvas.fill context
      Square ->
        Canvas.fillRect context { x: p.x - 4.0, y: p.y - 4.0, width: 8.0, height: 8.0 }
      Cross -> do
        Canvas.beginPath context
        Canvas.moveTo context (p.x - 4.0) p.y
        Canvas.lineTo context (p.x + 4.0) p.y
        Canvas.moveTo context p.x (p.y - 4.0)
        Canvas.lineTo context p.x (p.y + 4.0)
        Canvas.stroke context
      X -> do
        Canvas.beginPath context
        Canvas.moveTo context (p.x - 4.0) (p.y - 4.0)
        Canvas.lineTo context (p.x + 4.0) (p.y + 4.0)
        Canvas.moveTo context (p.x - 4.0) (p.y + 4.0)
        Canvas.lineTo context (p.x + 4.0) (p.y - 4.0)
        Canvas.stroke context

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
  Canvas.setStrokeStyle context (colorCss color)
  Canvas.setLineWidth context width
  Canvas.setGlobalAlpha context opacity
  Canvas.setLineDash context (dashPattern style)
  Canvas.setLineCap context (capStyle style)
  Canvas.beginPath context
  Canvas.moveTo context from.x from.y
  Canvas.lineTo context to.x to.y
  Canvas.stroke context
  Canvas.setGlobalAlpha context 1.0
  Canvas.setLineDash context []
  Canvas.setLineCap context Canvas.Butt

getCanvas :: Effect Canvas.CanvasElement
getCanvas = do
  maybeCanvas <- Canvas.getCanvasElementById "canvas-target"
  case maybeCanvas of
    Nothing -> throw "Missing #canvas-target canvas"
    Just canvas -> pure canvas

getContext :: Effect Canvas.Context2D
getContext = getCanvas >>= Canvas.getContext2D

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

dashPattern :: LineStyle -> Array Number
dashPattern Solid = []
dashPattern Dashed = [ 16.0, 12.0 ]
dashPattern Dotted = [ 0.0, 12.0 ]
dashPattern DashDot = [ 16.0, 8.0, 0.0, 8.0 ]

capStyle :: LineStyle -> Canvas.LineCap
capStyle Dotted = Canvas.Round
capStyle DashDot = Canvas.Round
capStyle _ = Canvas.Butt
