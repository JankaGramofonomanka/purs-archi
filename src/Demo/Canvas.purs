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
      Canvas.setFillStyle context "#5268d8"
      Canvas.fillRect context
        { x: 20.0
        , y: 70.0
        , width: 100.0
        , height: 100.0
        }
      Canvas.setFillStyle context "#e53935"
      Canvas.fillRect context
        { x: 120.0
        , y: 70.0
        , width: 100.0
        , height: 100.0
        }
