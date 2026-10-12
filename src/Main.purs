module Main where

import Prelude

import Effect (Effect)
import Canvas.Demo as Canvas
import Canvas.Clear as CanvasClear
import PureScriptDrawing.Demo as PureScriptDrawing
import PureScriptDrawing.Clear as PureScriptDrawingClear
import SVG.Demo as SVG
import SVG.Clear as SVGClear

main :: Effect Unit
main = setupRoutes drawRoute

drawRoute :: String -> Effect Unit
drawRoute route = case route of
  "/svg" -> SVGClear.clear
  "/demo/svg" -> SVG.draw
  "/canvas" -> CanvasClear.clear
  "/demo/canvas" -> Canvas.draw
  "/purescript-drawing" -> PureScriptDrawingClear.clear
  "/demo/purescript-drawing" -> PureScriptDrawing.draw
  _ -> pure unit

foreign import setupRoutes :: (String -> Effect Unit) -> Effect Unit
