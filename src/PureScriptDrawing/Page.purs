module PureScriptDrawing.Page (draw) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Graphics.Canvas as Canvas
import Renderer.Page as Page
import PureScriptDrawing.Impl as Renderer

draw :: Number -> Effect Unit
draw radius = do
  canvas <- getCanvas
  dimensions <- Canvas.getCanvasDimensions canvas
  context <- Canvas.getContext2D canvas
  Canvas.clearRect context { x: 0.0, y: 0.0, width: dimensions.width, height: dimensions.height }
  Renderer.runPSDRenderer (Page.draw dimensions.width dimensions.height radius)

getCanvas :: Effect Canvas.CanvasElement
getCanvas = do
  maybeCanvas <- Canvas.getCanvasElementById "drawing-target"
  case maybeCanvas of
    Nothing -> throw "Missing #drawing-target canvas"
    Just canvas -> pure canvas
