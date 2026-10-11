module Demo.PureScriptDrawing (draw) where

import Prelude

import Color (Color, rgb)
import Data.Array ((..))
import Data.Foldable (foldMap)
import Data.Int (ceil, toNumber)
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
            <> dashedLine dimensions.width (centerY - 350.0)
            <> dottedLine dimensions.width (centerY - 250.0)
            <> dashDotLine dimensions.width (centerY - 150.0)
            <> Drawing.outlined
                (Drawing.outlineColor ink <> Drawing.lineWidth 4.0)
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

ink :: Color
ink = rgb 28 37 64

dashLine :: Number -> Number -> Number -> Number -> Drawing.Drawing
dashLine width y dashWidth gap =
  foldMap drawDash (0 .. ceil (width / (dashWidth + gap)))
  where
  drawDash index =
    let x = toNumber index * (dashWidth + gap)
        visibleWidth = min dashWidth (width - x)
    in if x < width then
      Drawing.filled (Drawing.fillColor ink)
        (Drawing.rectangle x (y - 2.0) visibleWidth 4.0)
    else
      mempty

dashedLine :: Number -> Number -> Drawing.Drawing
dashedLine width y = dashLine width y 16.0 12.0

dottedLine :: Number -> Number -> Drawing.Drawing
dottedLine width y =
  foldMap drawDot (0 .. ceil (width / 12.0))
  where
  drawDot index =
    let x = toNumber index * 12.0
    in if x <= width then
      Drawing.filled (Drawing.fillColor ink) (Drawing.circle x y 2.0)
    else
      mempty

dashDotLine :: Number -> Number -> Drawing.Drawing
dashDotLine width y =
  foldMap drawDashDot (0 .. ceil (width / 32.0))
  where
  drawDashDot index =
    let x = toNumber index * 32.0
        dash =
          if x < width then
            Drawing.filled (Drawing.fillColor ink)
              (Drawing.rectangle x (y - 2.0) (min 16.0 (width - x)) 4.0)
          else
            mempty
        dot =
          if x + 24.0 <= width then
            Drawing.filled (Drawing.fillColor ink)
              (Drawing.circle (x + 24.0) y 2.0)
          else
            mempty
    in dash <> dot
