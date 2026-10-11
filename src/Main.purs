module Main where

import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import Demo.Canvas as Canvas
import Demo.PureScriptDrawing as PureScriptDrawing
import Demo.SVG as SVG

main :: Effect Unit
main = do
  setupRoutes drawRoute
  count <- Ref.new 0
  setCount 0
  onIncrement do
    next <- Ref.modify (_ + 1) count
    setCount next

drawRoute :: String -> Effect Unit
drawRoute route = case route of
  "/svg" -> SVG.draw
  "/canvas" -> Canvas.draw
  "/purescript-drawing" -> PureScriptDrawing.draw
  _ -> pure unit

foreign import setupRoutes :: (String -> Effect Unit) -> Effect Unit

foreign import setCount :: Int -> Effect Unit

foreign import onIncrement :: Effect Unit -> Effect Unit
