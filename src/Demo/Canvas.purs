module Demo.Canvas (draw) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Graphics.Canvas as Canvas

draw :: Effect Unit
draw = do
  maybeCanvas <- Canvas.getCanvasElementById "canvas-target"
  case maybeCanvas of
    Nothing -> throw "Missing #canvas-target canvas"
    Just canvas -> do
      context <- Canvas.getContext2D canvas
      dimensions <- Canvas.getCanvasDimensions canvas
      let centerX = dimensions.width / 2.0
      let centerY = dimensions.height / 2.0
      Canvas.setFillStyle context "#5268d8"
      Canvas.fillRect context
        { x: centerX - 100.0
        , y: centerY - 50.0
        , width: 100.0
        , height: 100.0
        }
      Canvas.setFillStyle context "#e53935"
      Canvas.fillRect context
        { x: centerX
        , y: centerY - 50.0
        , width: 100.0
        , height: 100.0
        }
      Canvas.beginPath context
      Canvas.moveTo context 0.0 (centerY - 50.0)
      Canvas.lineTo context dimensions.width (centerY - 50.0)
      Canvas.setStrokeStyle context "#1c2540"
      Canvas.setLineWidth context 2.0
      Canvas.stroke context
      Canvas.beginPath context
      Canvas.moveTo context (centerX + 100.0) (centerY - 150.0)
      Canvas.lineTo context (centerX + 100.0) (centerY + 150.0)
      Canvas.stroke context
