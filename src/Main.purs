module Main where

import Prelude

import Effect (Effect)
import Canvas.Demo as Canvas
import PureScriptDrawing.Demo as PureScriptDrawing
import SVG.Demo as SVG

main :: Effect Unit
main = setupRoutes drawRoute

drawRoute :: String -> Effect Unit
drawRoute route = case route of
  "/svg" -> SVG.draw
  "/canvas" -> Canvas.draw
  "/purescript-drawing" -> PureScriptDrawing.draw
  _ -> pure unit

foreign import setupRoutes :: (String -> Effect Unit) -> Effect Unit
