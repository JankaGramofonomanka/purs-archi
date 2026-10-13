module Canvas.Page (draw) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Graphics.Canvas as Canvas
import Renderer.Page as Page
import Canvas.Impl as Renderer
import Algo as Algo

draw :: Algo.State -> Effect Unit
draw algoState = do
  canvas <- getCanvas
  dimensions <- Canvas.getCanvasDimensions canvas
  context <- Canvas.getContext2D canvas
  Canvas.clearRect context { x: 0.0, y: 0.0, width: dimensions.width, height: dimensions.height }
  Renderer.runCanvasRenderer (Page.draw algoState)

getCanvas :: Effect Canvas.CanvasElement
getCanvas = do
  maybeCanvas <- Canvas.getCanvasElementById "canvas-target"
  case maybeCanvas of
    Nothing -> throw "Missing #canvas-target canvas"
    Just canvas -> pure canvas
