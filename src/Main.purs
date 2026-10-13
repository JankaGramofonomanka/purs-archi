module Main where

import Prelude

import Control.Monad (when)
import Control.Monad.State (runStateT)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Canvas.Demo as CanvasDemo
import Canvas.Page as CanvasPage
import PureScriptDrawing.Demo as PureScriptDrawingDemo
import PureScriptDrawing.Page as PureScriptDrawingPage
import SVG.Demo as SVGDemo
import SVG.Page as SVGPage
import Algo as Algo
import Test.QuickCheck.Gen as Gen

data Action = StepAlgorithm

type State =
  { route :: String
  , algoState :: Maybe Algo.State
  }

main :: Effect Unit
main = do
  state <- Ref.new { route: "", algoState: Nothing }
  setupRoutes (drawRoute state) (handleAction state)

drawRoute :: Ref.Ref State -> String -> Number -> Effect Unit
drawRoute state route surfaceWidth = do
  Ref.modify_ (_ { route = route }) state
  currentState <- Ref.read state
  if isBlankRoute route then do
    let algoState = case currentState.algoState of
          Nothing -> Algo.mkState (surfaceWidth * 0.25) (surfaceWidth * 0.75)
          Just existing -> existing
    Ref.modify_ (_ { algoState = Just algoState }) state
    renderBlankRoute route algoState
  else
    renderDemoRoute route

handleAction :: Ref.Ref State -> Effect Unit
handleAction state = handle StepAlgorithm
  where
  handle StepAlgorithm = do
    currentState <- Ref.read state
    when (isBlankRoute currentState.route) do
      case currentState.algoState of
        Nothing -> throw "Algorithm state has not been initialized"
        Just algoState -> do
          Tuple _ nextAlgoState <- Gen.randomSampleOne
            (runStateT Algo.step algoState)
          let nextState = currentState { algoState = Just nextAlgoState }
          Ref.write nextState state
          renderBlankRoute nextState.route nextAlgoState

isBlankRoute :: String -> Boolean
isBlankRoute route =
  route == "/svg" || route == "/canvas" || route == "/purescript-drawing"

renderBlankRoute :: String -> Algo.State -> Effect Unit
renderBlankRoute route algoState = case route of
  "/svg" -> SVGPage.draw algoState
  "/canvas" -> CanvasPage.draw algoState
  "/purescript-drawing" -> PureScriptDrawingPage.draw algoState
  _ -> pure unit

renderDemoRoute :: String -> Effect Unit
renderDemoRoute route = case route of
  "/demo/svg" -> SVGDemo.draw
  "/demo/canvas" -> CanvasDemo.draw
  "/demo/purescript-drawing" -> PureScriptDrawingDemo.draw
  _ -> pure unit

foreign import setupRoutes :: (String -> Number -> Effect Unit) -> Effect Unit -> Effect Unit
