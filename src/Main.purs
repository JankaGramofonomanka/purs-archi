module Main where

import Prelude

import Color (rgb)
import Data.Maybe (Maybe(..))
import Effect.Exception (throw)
import Effect (Effect)
import Effect.Ref as Ref
import Graphics.Canvas as Canvas
import Graphics.Drawing as Drawing

main :: Effect Unit
main = do
  setupRoutes
  drawCanvasSquare
  drawPureScriptDrawingSquare
  count <- Ref.new 0
  setCount 0
  onIncrement do
    next <- Ref.modify (_ + 1) count
    setCount next

foreign import setupRoutes :: Effect Unit

foreign import drawCanvasSquare :: Effect Unit

drawPureScriptDrawingSquare :: Effect Unit
drawPureScriptDrawingSquare = do
  maybeCanvas <- Canvas.getCanvasElementById "drawing-square"
  case maybeCanvas of
    Nothing -> throw "Missing #drawing-square canvas"
    Just canvas -> do
      context <- Canvas.getContext2D canvas
      Drawing.render context
        ( Drawing.filled
            (Drawing.fillColor (rgb 82 104 216))
            (Drawing.rectangle 60.0 60.0 120.0 120.0)
        )

foreign import setCount :: Int -> Effect Unit

foreign import onIncrement :: Effect Unit -> Effect Unit
