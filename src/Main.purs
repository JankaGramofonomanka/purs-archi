module Main where

import Prelude

import Control.Monad (when)
import Control.Monad.State (runStateT)
import Data.Array as Array
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Architecture.Order (Order, OrderKind(..), Treatise, treatiseOrder)
import Architecture.Order.Alberti as Alberti
import Architecture.Order.Chambers as Chambers
import Architecture.Order.Gibbs as Gibbs
import Architecture.Order.Palladio as Palladio
import Architecture.Order.Scamozzi as Scamozzi
import Architecture.Order.Serlio as Serlio
import Architecture.Order.Vignola as Vignola
import Architecture.Order.Vitruvius as Vitruvius
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

-- | The collonade settings as exchanged with the page's form: the key of a treatise from
-- | `treatises`, orders as keys from `orderKinds`, and the bay width in lower diameters.
type CollonadeForm = { treatise :: String, orders :: Array String, numOfColumns :: Int, bayWidth :: Number }

-- | A treatise as offered by the page's form, with the keys of the orders it defines.
type TreatiseOption = { key :: String, name :: String, orders :: Array String }

treatises :: Array { key :: String, name :: String, treatise :: Treatise }
treatises =
  [ { key: "vitruvius", name: "Vitruvius", treatise: Vitruvius.treatise }
  , { key: "alberti", name: "Alberti", treatise: Alberti.treatise }
  , { key: "serlio", name: "Serlio", treatise: Serlio.treatise }
  , { key: "vignola", name: "Vignola", treatise: Vignola.treatise }
  , { key: "palladio", name: "Palladio", treatise: Palladio.treatise }
  , { key: "scamozzi", name: "Scamozzi", treatise: Scamozzi.treatise }
  , { key: "gibbs", name: "Gibbs", treatise: Gibbs.treatise }
  , { key: "chambers", name: "Chambers", treatise: Chambers.treatise }
  ]

orderKinds :: Array { key :: String, kind :: OrderKind }
orderKinds =
  [ { key: "tuscan", kind: Tuscan }
  , { key: "doric", kind: Doric }
  , { key: "ionic", kind: Ionic }
  , { key: "corynthian", kind: Corynthian }
  , { key: "composite", kind: Composite }
  ]

treatiseOptions :: Array TreatiseOption
treatiseOptions = treatises <#> \{ key, name, treatise } ->
  { key
  , name
  , orders: _.key <$> Array.filter (\{ kind } -> isJust (treatiseOrder treatise kind)) orderKinds
  }

initialForm :: CollonadeForm
initialForm = { treatise: "palladio", orders: [ "doric", "ionic", "corynthian" ], numOfColumns: 5, bayWidth: 3.0 }

main :: Effect Unit
main = do
  initialCollonade <- fromForm initialForm
  state <- Ref.new { route: "", algoState: Nothing, collonade: initialCollonade }
  setupRoutes
    (drawRoute state)
    (handleAction state StepAlgorithm)
    treatiseOptions
    initialForm
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

fromForm :: CollonadeForm -> Effect Collonade.Config
fromForm form = do
  when (form.numOfColumns < 1) do
    throw "A collonade needs at least one column"
  when (form.bayWidth <= 0.0) do
    throw "The bay width must be positive"
  { name, treatise } <- case Array.find (\t -> t.key == form.treatise) treatises of
    Nothing -> throw ("Unknown treatise: " <> form.treatise)
    Just found -> pure found
  let
    orderFromKey :: String -> Effect Order
    orderFromKey key = case Array.find (\o -> o.key == key) orderKinds of
      Nothing -> throw ("Unknown order: " <> key)
      Just { kind } -> case treatiseOrder treatise kind of
        Nothing -> throw (name <> " does not define the " <> key <> " order")
        Just order -> pure order
  orders <- traverse orderFromKey form.orders
  pure { orders, numOfColumns: form.numOfColumns, bayWidth: form.bayWidth }

foreign import setupRoutes
  :: (String -> Number -> Number -> Effect Unit)
  -> Effect Unit
  -> Array TreatiseOption
  -> CollonadeForm
  -> (CollonadeForm -> Effect Unit)
  -> Effect Unit
