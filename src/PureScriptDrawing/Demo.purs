module PureScriptDrawing.Demo (draw) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Graphics.Canvas as Canvas
import Geometry.Demo as Demo
import PureScriptDrawing.Impl as Renderer

draw :: Effect Unit
draw = do
  maybeCanvas <- Canvas.getCanvasElementById "drawing-target"
  case maybeCanvas of
    Nothing -> throw "Missing #drawing-target canvas"
    Just canvas -> do
      dimensions <- Canvas.getCanvasDimensions canvas
      Renderer.runRenderer (Demo.draw dimensions.width dimensions.height)
