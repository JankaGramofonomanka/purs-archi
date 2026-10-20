module Main where

import Prelude

import Control.Monad (when)
import Control.Monad.State (runStateT)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Architecture (BayFill(..), BayMeasure(..), ColumnBeat(..), Storey)
import Architecture.Order (OrderKind(..), PedestalMode(..), Treatise, treatiseOrder)
import Architecture.Superposition (continuousTaper)
import Architecture.Order.Alberti as Alberti
import Architecture.Order.Chambers as Chambers
import Architecture.Order.Gibbs as Gibbs
import Architecture.Order.Palladio as Palladio
import Architecture.Order.Scamozzi as Scamozzi
import Architecture.Order.Serlio as Serlio
import Architecture.Order.Vignola as Vignola
import Architecture.Order.Vitruvius as Vitruvius
import Architecture.Rythm as Rythm
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
-- | `treatises`, orders as keys from `orderKinds` with a key from `pedestalModes` each, and
-- | the bay width in lower diameters with a key from `bayMeasures` saying what it measures. `rythm` is the number
-- | of columns standing on each of the `numOfColumns` beats (beats, in fact), and `bays` what fills each of the
-- | bays between them, left to right, as keys from `bayFills`, so there is one bay fewer than beats. `windowWidth`
-- | is the width of the windows' openings in lower diameters; with their architraves (a sixth of the opening
-- | on each side) they are at most as wide as the bay width less one diameter. `scale` is the lower diameter of the
-- | bottom row's columns in pixels and `offset` how far the drawing is moved from the center, in pixels.
type CollonadeForm =
  { treatise :: String
  , orders :: Array { order :: String, pedestal :: String }
  , numOfColumns :: Int
  , bayWidth :: Number
  , bayMeasure :: String
  , rythm :: Array Int
  , bays :: Array String
  , windowWidth :: Number
  , scale :: Number
  , offset :: { x :: Number, y :: Number }
  }

-- | A treatise as offered by the page's form, with the keys of the orders it defines and of
-- | those it gives a pedestal, and whether it gives a rule for stacking orders.
type TreatiseOption = { key :: String, name :: String, orders :: Array String, pedestals :: Array String, stacks :: Boolean }

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

bayMeasures :: Array { key :: String, measure :: BayMeasure }
bayMeasures =
  [ { key: "beats", measure: BetweenBeats }
  , { key: "groups", measure: BetweenGroups }
  ]

bayFills :: Array { key :: String, fill :: BayFill }
bayFills =
  [ { key: "wall", fill: Wall }
  , { key: "window", fill: Window }
  ]

pedestalModes :: Array { key :: String, mode :: PedestalMode }
pedestalModes =
  [ { key: "none", mode: NoPedestal }
  , { key: "pedestals", mode: Pedestals }
  , { key: "podium", mode: Podium }
  ]

treatiseOptions :: Array TreatiseOption
treatiseOptions = treatises <#> \{ key, name, treatise } ->
  { key
  , name
  , orders: _.key <$> Array.filter (\{ kind } -> isJust (treatiseOrder treatise kind)) orderKinds
  , pedestals: _.key <$> Array.filter (\{ kind } -> isJust (treatiseOrder treatise kind >>= _.pedestal)) orderKinds
  , stacks: isJust treatise.superposition
  }

initialForm :: CollonadeForm
initialForm = { treatise: "palladio", orders: [ "doric", "ionic", "corynthian" ] <#> { order: _, pedestal: "none" }, numOfColumns: 5, bayWidth: 3.0, bayMeasure: "beats", rythm: Array.replicate 5 1, bays: Array.replicate 4 "window", windowWidth: 1.0, scale: 10.0, offset: { x: 0.0, y: 0.0 } }

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
  when (form.windowWidth < 0.0) do
    throw "The window width cannot be negative"
  when (form.windowWidth * 4.0 / 3.0 > form.bayWidth - 1.0) do
    throw "A window with its architrave cannot be wider than the bay width less one diameter"
  when (form.scale <= 0.0) do
    throw "The scale of the drawing must be positive"
  { name, treatise } <- case Array.find (\t -> t.key == form.treatise) treatises of
    Nothing -> throw ("Unknown treatise: " <> form.treatise)
    Just found -> pure found
  let
    storeyFromForm :: { order :: String, pedestal :: String } -> Effect Storey
    storeyFromForm { order: key, pedestal: pedestalKey } = do
      order <- case Array.find (\o -> o.key == key) orderKinds of
        Nothing -> throw ("Unknown order: " <> key)
        Just { kind } -> case treatiseOrder treatise kind of
          Nothing -> throw (name <> " does not define the " <> key <> " order")
          Just order -> pure order
      pedestal <- case Array.find (\p -> p.key == pedestalKey) pedestalModes of
        Nothing -> throw ("Unknown pedestal mode: " <> pedestalKey)
        Just { mode } -> pure mode
      when (pedestal /= NoPedestal && isNothing order.pedestal) do
        throw (name <> " gives no pedestal for the " <> key <> " order")
      pure { order, pedestal }
  orders <- traverse storeyFromForm form.orders
  superposition <- case treatise.superposition of
    Just rule -> pure rule
    Nothing
      | Array.length orders > 1 -> throw (name <> " gives no rule for stacking orders")
      -- A single storey is never related to another, so any rule will do.
      | otherwise -> pure continuousTaper
  when (Array.length form.rythm /= form.numOfColumns) do
    throw "There must be as many beats as columns"
  when (Array.length form.bays /= form.numOfColumns - 1) do
    throw "There must be one bay fewer than beats"
  when (Array.any (_ < 0) form.rythm) do
    throw "A beat cannot have a negative number of columns"
  rythm <- case NonEmptyArray.fromArray (map ColumnBeat form.rythm) of
    Nothing -> throw "A collonade needs at least one beat"
    Just beats -> pure (Rythm.rythm beats)
  bayFillMotif <- traverse
    (\key -> case Array.find (\b -> b.key == key) bayFills of
        Nothing -> throw ("Unknown bay fill: " <> key)
        Just { fill } -> pure fill)
    form.bays
  -- A single beat has no bays, so what stands for them then is never looked at.
  let bays = Rythm.rythm (fromMaybe (NonEmptyArray.singleton Wall) (NonEmptyArray.fromArray bayFillMotif))
  bayMeasure <- case Array.find (\m -> m.key == form.bayMeasure) bayMeasures of
    Nothing -> throw ("Unknown bay measure: " <> form.bayMeasure)
    Just { measure } -> pure measure
  pure { orders, superposition, numOfColumns: form.numOfColumns, bayWidth: form.bayWidth, bayMeasure, rythm, bays, windowWidth: form.windowWidth, lowestLowerDiameter: form.scale, offset: form.offset }

foreign import setupRoutes
  :: (String -> Number -> Number -> Effect Unit)
  -> Effect Unit
  -> Array TreatiseOption
  -> CollonadeForm
  -> (CollonadeForm -> Effect Unit)
  -> Effect Unit
