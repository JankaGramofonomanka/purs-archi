module Main where

import Prelude

import Control.Monad.State (runStateT)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Architecture (ArcadeMode(..), BayFill(..), BayMeasure(..), ColumnBeat(..), archesTooTall)
import Architecture.Order (Order, OrderKind(..), PedestalMode(..), Treatise, arcadeOf, minuteFromLowerDiameter, sizeOf, treatiseOrder, upperMinute)
import Architecture.Superposition (SuperpositionRule, continuousTaper)
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
-- | `treatises`, orders as keys from `orderKinds` with a key from `pedestalModes` each and whether their
-- | columns frame an arcade, and the bay width in lower diameters with a key from `bayMeasures` saying what it measures. `rythm` is the number
-- | of columns standing on each of the `numOfColumns` beats (beats, in fact), and `bays` what fills each of the
-- | bays between them, left to right, as keys from `bayFills`, so there is one bay fewer than beats. `windowWidth`
-- | is the width of the windows' openings in lower diameters; with their architraves (a sixth of the opening
-- | on each side) they are at most as wide as the bay width less one diameter. `scale` is the lower diameter of the
-- | bottom row's columns in pixels and `offset` how far the drawing is moved from the center, in pixels.
-- | While a storey frames an arcade, a `bayWidth` less than `arcadeBayWidth` is taken as that. `arcadeMode`, a key
-- | from `arcadeModes`, says how wide the arches above the lowest arcade are.
type CollonadeForm =
  { treatise :: String
  , orders :: Array FormStorey
  , numOfColumns :: Int
  , bayWidth :: Number
  , bayMeasure :: String
  , rythm :: Array Int
  , bays :: Array String
  , windowWidth :: Number
  , scale :: Number
  , offset :: { x :: Number, y :: Number }
  , arcadeMode :: String
  }

type FormStorey = { order :: String, pedestal :: String, arcade :: Boolean }

-- | A treatise as offered by the page's form, with the keys of the orders it defines, of those
-- | it gives a pedestal, and of those it gives an arcade without and with pedestals, and whether
-- | it gives a rule for stacking orders.
type TreatiseOption =
  { key :: String
  , name :: String
  , orders :: Array String
  , pedestals :: Array String
  , arcades :: Array String
  , pedestalArcades :: Array String
  , stacks :: Boolean
  }

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

arcadeModes :: Array { key :: String, mode :: ArcadeMode }
arcadeModes =
  [ { key: "arches", mode: OrderArches }
  , { key: "width", mode: ConstantArchWidth }
  , { key: "piers", mode: OrderPiers }
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
  , pedestals: _.key <$> Array.filter (\{ kind } -> isJust (treatiseOrder treatise kind >>= _.pedestal <<< unwrap)) orderKinds
  , arcades: _.key <$> Array.filter (\{ kind } -> isJust (treatiseOrder treatise kind >>= _.arcadeNoPedestal <<< unwrap)) orderKinds
  , pedestalArcades: _.key <$> Array.filter (\{ kind } -> isJust (treatiseOrder treatise kind >>= _.arcadePedestal <<< unwrap)) orderKinds
  , stacks: isJust treatise.superposition
  }

initialForm :: CollonadeForm
initialForm = { treatise: "palladio", orders: [ "doric", "ionic", "corynthian" ] <#> { order: _, pedestal: "none", arcade: false }, numOfColumns: 5, bayWidth: 3.0, bayMeasure: "groups", rythm: Array.replicate 5 1, bays: Array.replicate 4 "window", windowWidth: 1.0, scale: 10.0, offset: { x: 0.0, y: 0.0 }, arcadeMode: "arches" }

main :: Effect Unit
main = do
  initialCollonade <- fromForm initialForm
  state <- Ref.new { route: "", algoState: Nothing, collonade: initialCollonade }
  setupRoutes
    (drawRoute state)
    (handleAction state StepAlgorithm)
    treatiseOptions
    formStatus
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
  route == "/algo/svg" || route == "/algo/canvas" || route == "/algo/purescript-drawing"

isCollonadeRoute :: String -> Boolean
isCollonadeRoute route =
  route == "/collonade/svg" || route == "/collonade/canvas" || route == "/collonade/purescript-drawing"

renderBlankRoute :: String -> Algo.State -> Effect Unit
renderBlankRoute route algoState = case route of
  "/algo/svg" -> SVGPage.draw algoState
  "/algo/canvas" -> CanvasPage.draw algoState
  "/algo/purescript-drawing" -> PureScriptDrawingPage.draw algoState
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

-- | The storeys of the form, as the treatise it names proportions them, and the rule for stacking them.
type FormStoreys =
  { storeys :: Array { order :: Order, pedestal :: PedestalMode, arcade :: Boolean }
  , superposition :: SuperpositionRule
  }

formStoreys :: CollonadeForm -> Either String FormStoreys
formStoreys form = do
  { name, treatise } <- case Array.find (\t -> t.key == form.treatise) treatises of
    Nothing -> Left ("Unknown treatise: " <> form.treatise)
    Just found -> Right found
  let
    storeyFromForm { order: key, pedestal: pedestalKey, arcade } = do
      order <- case Array.find (\o -> o.key == key) orderKinds of
        Nothing -> Left ("Unknown order: " <> key)
        Just { kind } -> case treatiseOrder treatise kind of
          Nothing -> Left (name <> " does not define the " <> key <> " order")
          Just order -> Right order
      pedestal <- case Array.find (\p -> p.key == pedestalKey) pedestalModes of
        Nothing -> Left ("Unknown pedestal mode: " <> pedestalKey)
        Just { mode } -> Right mode
      when (pedestal /= NoPedestal && isNothing (unwrap order).pedestal) do
        Left (name <> " gives no pedestal for the " <> key <> " order")
      when (arcade && isNothing (arcadeOf pedestal order)) do
        Left (name <> " gives no arcade for the " <> key <> " order " <> if pedestal == NoPedestal then "without pedestals" else "with pedestals")
      pure { order, pedestal, arcade }
  storeys <- traverse storeyFromForm form.orders
  superposition <- case treatise.superposition of
    Just rule -> Right rule
    Nothing
      | Array.length storeys > 1 -> Left (name <> " gives no rule for stacking orders")
      -- A single storey is never related to another, so any rule will do.
      | otherwise -> Right continuousTaper
  pure { storeys, superposition }

-- | The bay width, in lower diameters of the bottom row's columns, that follows from the arches:
-- | an opening and a pier of the lowest storey that frames an arcade, which "gives the rule to the
-- | other" storeys (Gibbs). It is measured as `bayMeasure` says: between single columns it is the
-- | distance between their axes, and between groups the distance between the facing columns, which
-- | the arch spans. `Nothing` where no storey does.
arcadeBayWidth :: FormStoreys -> Maybe Number
arcadeBayWidth { storeys, superposition } = do
  { index, storey } <- Array.find (_.storey.arcade) (Array.mapWithIndex { index: _, storey: _ } storeys)
  arcade <- arcadeOf storey.pedestal storey.order
  minute <- Array.index rowMinutes index
  pure (sizeOf arcade.openingWidth minute + sizeOf arcade.pierWidth minute)
  where
  rows = storeys <#> \{ order, pedestal } -> { order, pedestal }
  -- The minute of each storey, in lower diameters of the bottom row's columns.
  rowMinutes = foldl nextMinute [] (Array.mapWithIndex Tuple rows)
  nextMinute minutes (Tuple index row) = Array.snoc minutes $
    case Array.last minutes, Array.index rows (index - 1) of
      Just minute, Just lower -> upperMinute superposition (index - 1) lower minute row
      _, _ -> minuteFromLowerDiameter row.order 1.0

-- | What the page's form shows that follows from its settings: whether any storey frames an arcade
-- | (`arcades`), and then the least bay width it allows (`minBayWidth`), the bay width drawn
-- | (`bayWidth`: the one typed in, but no less than that), and for each storey, bottom to top,
-- | whether its arches would be too tall for its columns
-- | (`archTooTall`; see `archesTooTall`). A form that is not valid has no arcades.
formStatus :: CollonadeForm -> { arcades :: Boolean, minBayWidth :: Number, bayWidth :: Number, archTooTall :: Array Boolean }
formStatus form =
  { arcades: isJust arcadeBay
  , minBayWidth: fromMaybe 0.0 arcadeBay
  , bayWidth: maybe form.bayWidth (max form.bayWidth) arcadeBay
  , archTooTall: either (const (false <$ form.orders)) (archesTooTall <<< Collonade.stackedCollonade) (configFromForm form)
  }
  where
  arcadeBay = either (const Nothing) arcadeBayWidth (formStoreys form)

fromForm :: CollonadeForm -> Effect Collonade.Config
fromForm = either throw pure <<< configFromForm

configFromForm :: CollonadeForm -> Either String Collonade.Config
configFromForm form = do
  parsed <- formStoreys form
  let arcadeBay = arcadeBayWidth parsed
  -- The arches set the least bay width; a wider one widens the piers.
  let bayWidth = maybe form.bayWidth (max form.bayWidth) arcadeBay
  when (Array.length form.orders < 1) do
    Left "A collonade needs at least one order"
  when (form.numOfColumns < 1) do
    Left "A collonade needs at least one column"
  when (bayWidth <= 0.0) do
    Left "The bay width must be positive"
  when (form.windowWidth < 0.0) do
    Left "The window width cannot be negative"
  when (form.windowWidth * 4.0 / 3.0 > bayWidth - 1.0) do
    Left "A window with its architrave cannot be wider than the bay width less one diameter"
  when (form.scale <= 0.0) do
    Left "The scale of the drawing must be positive"
  let orders = parsed.storeys
  let superposition = parsed.superposition
  when (Array.length form.rythm /= form.numOfColumns) do
    Left "There must be as many axes as columns"
  when (Array.length form.bays /= form.numOfColumns - 1) do
    Left "There must be one bay fewer than axes"
  when (Array.any (_ < 0) form.rythm) do
    Left "An axis cannot have a negative number of columns"
  rythm <- case NonEmptyArray.fromArray (map ColumnBeat form.rythm) of
    Nothing -> Left "A collonade needs at least one axis"
    Just beats -> pure (Rythm.rythm beats)
  bayFillMotif <- traverse
    (\key -> case Array.find (\b -> b.key == key) bayFills of
        Nothing -> Left ("Unknown bay fill: " <> key)
        Just { fill } -> pure fill)
    form.bays
  -- A single beat has no bays, so what stands for them then is never looked at.
  let bays = Rythm.rythm (fromMaybe (NonEmptyArray.singleton Wall) (NonEmptyArray.fromArray bayFillMotif))
  bayMeasure <- case Array.find (\m -> m.key == form.bayMeasure) bayMeasures of
    Nothing -> Left ("Unknown bay measure: " <> form.bayMeasure)
    Just { measure } -> pure measure
  arcadeMode <- case Array.find (\m -> m.key == form.arcadeMode) arcadeModes of
    Nothing -> Left ("Unknown arcade mode: " <> form.arcadeMode)
    Just { mode } -> pure mode
  pure { orders, superposition, numOfColumns: form.numOfColumns, bayWidth, bayMeasure, rythm, bays, windowWidth: form.windowWidth, lowestLowerDiameter: form.scale, offset: form.offset, arcadeMode }

foreign import setupRoutes
  :: (String -> Number -> Number -> Effect Unit)
  -> Effect Unit
  -> Array TreatiseOption
  -> (CollonadeForm -> { arcades :: Boolean, minBayWidth :: Number, bayWidth :: Number, archTooTall :: Array Boolean })
  -> CollonadeForm
  -> (CollonadeForm -> Effect Unit)
  -> Effect Unit
