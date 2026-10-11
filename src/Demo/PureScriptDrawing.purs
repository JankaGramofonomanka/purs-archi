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
        ( Drawing.filled (Drawing.fillColor (rgb 82 104 216))
            (Drawing.rectangle 20.0 70.0 100.0 100.0)
            <> Drawing.filled (Drawing.fillColor (rgb 229 57 53))
              (Drawing.rectangle 120.0 70.0 100.0 100.0)
        )
