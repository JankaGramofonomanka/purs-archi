module Renderer.Collonade (Config, draw, stackedCollonade) where

import Prelude

import Anchor (placeCentered, render)
import Architecture (ArcadeMode, BayFill, BayMeasure, ColumnBeat, StackedCollonade(..), Storey)
import Architecture.Measure (Measure(..))
import Architecture.Rythm (Rythm)
import Architecture.Superposition (SuperpositionRule)
import Renderer (point)
import Renderer as Renderer

-- | Storeys (orders and how their columns stand) listed from bottom to top; `bayWidth` is the distance between column centerlines,
-- | in lower diameters of the bottom row's columns, measured as `bayMeasure` says; `rythm` tells how many columns
-- | stand on each of the `numOfColumns` beats and `bays` what fills each bay between them; `windowWidth` is the width of
-- | the windows' openings in lower diameters of the bottom row's columns; `superposition` sizes each storey from the one below;
-- | `lowestLowerDiameter` is the lower diameter of the bottom row's columns in pixels, the scale of the drawing,
-- | and `offset` how far, in pixels, the drawing is moved from the center of the surface; `arcadeMode` says
-- | how wide the arches above the lowest arcade are.
type Config = { orders :: Array Storey, superposition :: SuperpositionRule, numOfColumns :: Int, bayWidth :: Number, bayMeasure :: BayMeasure, rythm :: Rythm ColumnBeat, bays :: Rythm BayFill, windowWidth :: Number, lowestLowerDiameter :: Number, offset :: { x :: Number, y :: Number }, arcadeMode :: ArcadeMode }

-- | Draw the stacked collonade centered on the surface, moved by `offset`.
draw :: forall m. Monad m => Renderer.Renderer m => Config -> Number -> Number -> m Unit
draw config width height =
  render $ placeCentered (point (width / 2.0 + config.offset.x) (height / 2.0 + config.offset.y)) $ stackedCollonade config

-- | The stacked collonade the configuration describes, in pixels.
stackedCollonade :: Config -> StackedCollonade
stackedCollonade { orders, superposition, numOfColumns, bayWidth, bayMeasure, rythm, bays, windowWidth, lowestLowerDiameter, arcadeMode } =
  StackedCollonade
    { orders
    , superposition
    , size: { row: 0, size: { measure: LowerDiameter, value: lowestLowerDiameter } }
    , numOfColumns
    , bayWidth: bayWidth * lowestLowerDiameter
    , bayMeasure
    , rythm
    , bays
    , windowWidth: windowWidth * lowestLowerDiameter
    , arcadeMode
    }
