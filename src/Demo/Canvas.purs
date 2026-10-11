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
      Canvas.setLineWidth context 4.0
      Canvas.stroke context
      Canvas.beginPath context
      Canvas.setStrokeStyle context "#c2410c"
      Canvas.setLineDash context [ 16.0, 12.0 ]
      Canvas.moveTo context 0.0 (centerY - 350.0)
      Canvas.lineTo context dimensions.width (centerY - 350.0)
      Canvas.stroke context
      Canvas.beginPath context
      Canvas.setStrokeStyle context "#0f766e"
      Canvas.setLineDash context [ 0.0, 12.0 ]
      Canvas.setLineCap context Canvas.Round
      Canvas.moveTo context 0.0 (centerY - 250.0)
      Canvas.lineTo context dimensions.width (centerY - 250.0)
      Canvas.stroke context
      Canvas.setLineCap context Canvas.Butt
      Canvas.beginPath context
      Canvas.setStrokeStyle context "#7e22ce"
      Canvas.setLineDash context [ 16.0, 8.0, 0.0, 8.0 ]
      Canvas.setLineCap context Canvas.Round
      Canvas.moveTo context 0.0 (centerY - 150.0)
      Canvas.lineTo context dimensions.width (centerY - 150.0)
      Canvas.stroke context
      Canvas.setLineDash context []
      Canvas.setLineCap context Canvas.Butt
      Canvas.setStrokeStyle context "#1c2540"
      Canvas.beginPath context
      Canvas.moveTo context (centerX + 100.0) (centerY - 150.0)
      Canvas.lineTo context (centerX + 100.0) (centerY + 150.0)
      Canvas.setGlobalAlpha context 1.0
      Canvas.stroke context
      Canvas.setLineWidth context 6.0
      Canvas.beginPath context
      Canvas.moveTo context (centerX + 200.0) (centerY - 150.0)
      Canvas.lineTo context (centerX + 200.0) (centerY + 150.0)
      Canvas.setGlobalAlpha context 0.1
      Canvas.stroke context
      Canvas.setLineWidth context 4.0
      Canvas.beginPath context
      Canvas.moveTo context (centerX + 300.0) (centerY - 150.0)
      Canvas.lineTo context (centerX + 300.0) (centerY + 150.0)
      Canvas.setGlobalAlpha context 0.3
      Canvas.stroke context
      Canvas.setLineWidth context 2.0
      Canvas.beginPath context
      Canvas.moveTo context (centerX + 400.0) (centerY - 150.0)
      Canvas.lineTo context (centerX + 400.0) (centerY + 150.0)
      Canvas.setGlobalAlpha context 1.0
      Canvas.stroke context
      Canvas.setGlobalAlpha context 1.0
