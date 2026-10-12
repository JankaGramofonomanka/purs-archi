module PureScriptDrawing.Clear (clear) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Graphics.Canvas as Canvas

clear :: Effect Unit
clear = do
  canvas <- getCanvas
  dimensions <- Canvas.getCanvasDimensions canvas
  context <- Canvas.getContext2D canvas
  Canvas.clearRect context { x: 0.0, y: 0.0, width: dimensions.width, height: dimensions.height }

getCanvas :: Effect Canvas.CanvasElement
getCanvas = do
  maybeCanvas <- Canvas.getCanvasElementById "drawing-target"
  case maybeCanvas of
    Nothing -> throw "Missing #drawing-target canvas"
    Just canvas -> pure canvas
