module Canvas.Demo (draw) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Exception (throw)
import Effect (Effect)
import Graphics.Canvas as Canvas
import Renderer.Demo as Demo
import Canvas.Impl as Renderer

draw :: Effect Unit
draw = do
  maybeCanvas <- Canvas.getCanvasElementById "canvas-target"
  case maybeCanvas of
    Nothing -> throw "Missing #canvas-target canvas"
    Just canvas -> do
      dimensions <- Canvas.getCanvasDimensions canvas
      Renderer.runCanvasRenderer (Demo.draw dimensions.width dimensions.height)
