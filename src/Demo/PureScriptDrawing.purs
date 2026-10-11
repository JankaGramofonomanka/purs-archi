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
      dimensions <- Canvas.getCanvasDimensions canvas
      let centerX = dimensions.width / 2.0
      let centerY = dimensions.height / 2.0
      Drawing.render context
        ( Drawing.filled (Drawing.fillColor (rgb 82 104 216))
            (Drawing.rectangle (centerX - 100.0) (centerY - 50.0) 100.0 100.0)
            <> Drawing.filled (Drawing.fillColor (rgb 229 57 53))
              (Drawing.rectangle centerX (centerY - 50.0) 100.0 100.0)
            <> Drawing.outlined
              (Drawing.outlineColor (rgb 28 37 64) <> Drawing.lineWidth 4.0)
              (Drawing.path
                [ { x: 0.0, y: centerY - 50.0 }
                , { x: dimensions.width, y: centerY - 50.0 }
                ]
              )
            <> Drawing.outlined
              (Drawing.outlineColor (rgb 28 37 64) <> Drawing.lineWidth 4.0)
              (Drawing.path
                [ { x: centerX + 100.0, y: centerY - 150.0 }
                , { x: centerX + 100.0, y: centerY + 150.0 }
                ]
              )
            <> Drawing.outlined
              (Drawing.outlineColor (rgb 28 37 64) <> Drawing.lineWidth 6.0)
              (Drawing.path
                [ { x: centerX + 200.0, y: centerY - 150.0 }
                , { x: centerX + 200.0, y: centerY + 150.0 }
                ]
              )
            <> Drawing.outlined
              (Drawing.outlineColor (rgb 28 37 64) <> Drawing.lineWidth 4.0)
              (Drawing.path
                [ { x: centerX + 300.0, y: centerY - 150.0 }
                , { x: centerX + 300.0, y: centerY + 150.0 }
                ]
              )
            <> Drawing.outlined
              (Drawing.outlineColor (rgb 28 37 64) <> Drawing.lineWidth 2.0)
              (Drawing.path
                [ { x: centerX + 400.0, y: centerY - 150.0 }
                , { x: centerX + 400.0, y: centerY + 150.0 }
                ]
              )
        )
