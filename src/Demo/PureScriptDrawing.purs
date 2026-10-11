module Demo.PureScriptDrawing (draw) where

import Prelude

import Color (Color, rgb, rgba)
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
            <> dashedLine (rgb 194 65 12) dimensions.width (centerY - 350.0)
            <> dottedLine (rgb 15 118 110) dimensions.width (centerY - 250.0)
            <> dashDotLine (rgb 126 34 206) dimensions.width (centerY - 150.0)
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
              (Drawing.outlineColor (rgba 28 37 64 0.1) <> Drawing.lineWidth 6.0)
              (Drawing.path
                [ { x: centerX + 200.0, y: centerY - 150.0 }
                , { x: centerX + 200.0, y: centerY + 150.0 }
                ]
              )
            <> Drawing.outlined
              (Drawing.outlineColor (rgba 28 37 64 0.3) <> Drawing.lineWidth 4.0)
              (Drawing.path
                [ { x: centerX + 300.0, y: centerY - 150.0 }
                , { x: centerX + 300.0, y: centerY + 150.0 }
                ]
              )
            <> Drawing.outlined
              (Drawing.outlineColor (rgba 28 37 64 1.0) <> Drawing.lineWidth 2.0)
              (Drawing.path
                [ { x: centerX + 400.0, y: centerY - 150.0 }
                , { x: centerX + 400.0, y: centerY + 150.0 }
                ]
              )
            <> Drawing.filled (Drawing.fillColor ink)
              (Drawing.circle (centerX - 200.0) (centerY + 50.0) 4.0)
            <> Drawing.outlined (Drawing.outlineColor ink <> Drawing.lineWidth 2.0)
              (Drawing.path
                [ { x: centerX - 104.0, y: centerY + 50.0 }
                , { x: centerX - 96.0, y: centerY + 50.0 }
                ]
              )
            <> Drawing.outlined (Drawing.outlineColor ink <> Drawing.lineWidth 2.0)
              (Drawing.path
                [ { x: centerX - 100.0, y: centerY + 46.0 }
                , { x: centerX - 100.0, y: centerY + 54.0 }
                ]
              )
            <> Drawing.filled (Drawing.fillColor ink)
              (Drawing.rectangle (centerX - 204.0) (centerY + 146.0) 8.0 8.0)
            <> Drawing.outlined (Drawing.outlineColor ink <> Drawing.lineWidth 2.0)
              (Drawing.path
                [ { x: centerX - 104.0, y: centerY + 146.0 }
                , { x: centerX - 96.0, y: centerY + 154.0 }
                ]
              )
            <> Drawing.outlined (Drawing.outlineColor ink <> Drawing.lineWidth 2.0)
              (Drawing.path
                [ { x: centerX - 104.0, y: centerY + 154.0 }
                , { x: centerX - 96.0, y: centerY + 146.0 }
                ]
              )
        )

ink :: Color
ink = rgb 28 37 64

dashLine :: Color -> Number -> Number -> Number -> Number -> Drawing.Drawing
dashLine color width y dashWidth gap =
  foldMap drawDash (0 .. ceil (width / (dashWidth + gap)))
  where
  drawDash index =
    let x = toNumber index * (dashWidth + gap)
        visibleWidth = min dashWidth (width - x)
    in if x < width then
      Drawing.filled (Drawing.fillColor color)
        (Drawing.rectangle x (y - 2.0) visibleWidth 4.0)
    else
      mempty

dashedLine :: Color -> Number -> Number -> Drawing.Drawing
dashedLine color width y = dashLine color width y 16.0 12.0

dottedLine :: Color -> Number -> Number -> Drawing.Drawing
dottedLine color width y =
  foldMap drawDot (0 .. ceil (width / 12.0))
  where
  drawDot index =
    let x = toNumber index * 12.0
    in if x <= width then
      Drawing.filled (Drawing.fillColor color) (Drawing.circle x y 2.0)
    else
      mempty

dashDotLine :: Color -> Number -> Number -> Drawing.Drawing
dashDotLine color width y =
  foldMap drawDashDot (0 .. ceil (width / 32.0))
  where
  drawDashDot index =
    let x = toNumber index * 32.0
        dash =
          if x < width then
            Drawing.filled (Drawing.fillColor color)
              (Drawing.rectangle x (y - 2.0) (min 16.0 (width - x)) 4.0)
          else
            mempty
        dot =
          if x + 24.0 <= width then
            Drawing.filled (Drawing.fillColor color)
              (Drawing.circle (x + 24.0) y 2.0)
          else
            mempty
    in dash <> dot
