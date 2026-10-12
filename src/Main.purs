module Main where

import Prelude

import Control.Monad (when)
import Effect (Effect)
import Effect.Ref as Ref
import Canvas.Demo as CanvasDemo
import Canvas.Page as CanvasPage
import PureScriptDrawing.Demo as PureScriptDrawingDemo
import PureScriptDrawing.Page as PureScriptDrawingPage
import SVG.Demo as SVGDemo
import SVG.Page as SVGPage

data Action = IncreaseRadius

type State =
  { radius :: Number
  , route :: String
  }

main :: Effect Unit
main = do
  state <- Ref.new { radius: 10.0, route: "" }
  setupRoutes (drawRoute state) (handleAction state)

drawRoute :: Ref.Ref State -> String -> Effect Unit
drawRoute state route = do
  Ref.modify_ (_ { route = route }) state
  currentState <- Ref.read state
  renderRoute route currentState.radius

handleAction :: Ref.Ref State -> Effect Unit
handleAction state = do
  currentState <- Ref.read state
  when (isBlankRoute currentState.route) do
    let nextState = reduce IncreaseRadius currentState
    Ref.write nextState state
    renderRoute nextState.route nextState.radius

reduce :: Action -> State -> State
reduce IncreaseRadius state = state { radius = state.radius + 10.0 }

isBlankRoute :: String -> Boolean
isBlankRoute route =
  route == "/svg" || route == "/canvas" || route == "/purescript-drawing"

renderRoute :: String -> Number -> Effect Unit
renderRoute route radius = case route of
  "/svg" -> SVGPage.draw radius
  "/demo/svg" -> SVGDemo.draw
  "/canvas" -> CanvasPage.draw radius
  "/demo/canvas" -> CanvasDemo.draw
  "/purescript-drawing" -> PureScriptDrawingPage.draw radius
  "/demo/purescript-drawing" -> PureScriptDrawingDemo.draw
  _ -> pure unit

foreign import setupRoutes :: (String -> Effect Unit) -> Effect Unit -> Effect Unit
