module Demo.PureScriptDrawing (draw) where

import Prelude

import Color (rgb)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Graphics.Canvas as Canvas
import Graphics.Drawing as Drawing

draw :: Effect Unit
draw = do
  maybeCanvas <- Canvas.getCanvasElementById "drawing-target"
  case maybeCanvas of
    Nothing -> throw "Missing #drawing-target canvas"
    Just canvas -> do
      context <- Canvas.getContext2D canvas
      Drawing.render context
        ( Drawing.filled
            (Drawing.fillColor (rgb 82 104 216))
            (Drawing.rectangle 60.0 60.0 120.0 120.0)
        )
