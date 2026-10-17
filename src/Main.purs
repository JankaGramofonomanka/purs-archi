module Main where

import Prelude

import Control.Monad (when)
import Control.Monad.State (runStateT)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Architecture.Order (Order, OrderKind(..), corynthian, doric, ionic)
import Canvas.Collonade as CanvasCollonade
import Canvas.Demo as CanvasDemo
import Canvas.Page as CanvasPage
import PureScriptDrawing.Collonade as PureScriptDrawingCollonade
import PureScriptDrawing.Demo as PureScriptDrawingDemo
import PureScriptDrawing.Page as PureScriptDrawingPage
import Renderer.Collonade as Collonade
import SVG.Collonade as SVGCollonade
import SVG.Demo as SVGDemo
import SVG.Page as SVGPage
import Algo as Algo
import Test.QuickCheck.Gen as Gen

data Action = StepAlgorithm | SetCollonade Collonade.Config

type State =
  { route :: String
  , algoState :: Maybe Algo.State
  , collonade :: Collonade.Config
  }

-- | The collonade settings as exchanged with the page's form; orders are keys from `orderKey`.
type CollonadeForm = { orders :: Array String, numOfColumns :: Int }

initialCollonade :: Collonade.Config
initialCollonade = { orders: [ doric, ionic, corynthian ], numOfColumns: 5 }

main :: Effect Unit
main = do
  state <- Ref.new { route: "", algoState: Nothing, collonade: initialCollonade }
  setupRoutes
    (drawRoute state)
    (handleAction state StepAlgorithm)
    (toForm initialCollonade)
    (\form -> fromForm form >>= SetCollonade >>> handleAction state)

drawRoute :: Ref.Ref State -> String -> Number -> Number -> Effect Unit
drawRoute state route surfaceWidth surfaceHeight = do
  Ref.modify_ (_ { route = route }) state
  currentState <- Ref.read state
  if isBlankRoute route then do
    let algoState = case currentState.algoState of
          Nothing -> Algo.mkState (surfaceWidth * 0.25) (surfaceWidth * 0.75) (surfaceHeight * 0.25) (surfaceHeight * 0.75)
          Just existing -> existing
    Ref.modify_ (_ { algoState = Just algoState }) state
    renderBlankRoute route algoState
  else if isCollonadeRoute route then
    renderCollonadeRoute route currentState.collonade
  else
    renderDemoRoute route

handleAction :: Ref.Ref State -> Action -> Effect Unit
handleAction state = case _ of
  StepAlgorithm -> do
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
  SetCollonade collonade -> do
    nextState <- Ref.modify (_ { collonade = collonade }) state
    when (isCollonadeRoute nextState.route) do
      renderCollonadeRoute nextState.route collonade

isBlankRoute :: String -> Boolean
isBlankRoute route =
  route == "/svg" || route == "/canvas" || route == "/purescript-drawing"

isCollonadeRoute :: String -> Boolean
isCollonadeRoute route =
  route == "/collonade/svg" || route == "/collonade/canvas" || route == "/collonade/purescript-drawing"

renderBlankRoute :: String -> Algo.State -> Effect Unit
renderBlankRoute route algoState = case route of
  "/svg" -> SVGPage.draw algoState
  "/canvas" -> CanvasPage.draw algoState
  "/purescript-drawing" -> PureScriptDrawingPage.draw algoState
  _ -> pure unit

renderCollonadeRoute :: String -> Collonade.Config -> Effect Unit
renderCollonadeRoute route collonade = case route of
  "/collonade/svg" -> SVGCollonade.draw collonade
  "/collonade/canvas" -> CanvasCollonade.draw collonade
  "/collonade/purescript-drawing" -> PureScriptDrawingCollonade.draw collonade
  _ -> pure unit

renderDemoRoute :: String -> Effect Unit
renderDemoRoute route = case route of
  "/demo/svg" -> SVGDemo.draw
  "/demo/canvas" -> CanvasDemo.draw
  "/demo/purescript-drawing" -> PureScriptDrawingDemo.draw
  _ -> pure unit

orderKey :: Order -> String
orderKey order = case order.kind of
  Doric -> "doric"
  Ionic -> "ionic"
  Corynthian -> "corynthian"

orderFromKey :: String -> Effect Order
orderFromKey = case _ of
  "doric" -> pure doric
  "ionic" -> pure ionic
  "corynthian" -> pure corynthian
  key -> throw ("Unknown order: " <> key)

toForm :: Collonade.Config -> CollonadeForm
toForm collonade = collonade { orders = map orderKey collonade.orders }

fromForm :: CollonadeForm -> Effect Collonade.Config
fromForm form = do
  when (form.numOfColumns < 1) do
    throw "A collonade needs at least one column"
  orders <- traverse orderFromKey form.orders
  pure form { orders = orders }

foreign import setupRoutes
  :: (String -> Number -> Number -> Effect Unit)
  -> Effect Unit
  -> CollonadeForm
  -> (CollonadeForm -> Effect Unit)
  -> Effect Unit
