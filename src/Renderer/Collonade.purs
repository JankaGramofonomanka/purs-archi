module Renderer.Collonade (Config, draw) where

import Prelude

import Anchor (placeCentered, render)
import Architecture (BayMeasure, ColumnBeat, StackedCollonade(..), Storey)
import Architecture.Measure (Measure(..))
import Architecture.Rythm (Rythm)
import Architecture.Superposition (SuperpositionRule)
import Renderer (point)
import Renderer as Renderer

-- | Storeys (orders and how their columns stand) listed from bottom to top; `bayWidth` is the distance between column centerlines,
-- | in lower diameters of the bottom row's columns, measured as `bayMeasure` says; `rythm` tells how many columns
-- | stand on each of the `numOfColumns` beats; `superposition` sizes each storey from the one below;
-- | `lowestLowerDiameter` is the lower diameter of the bottom row's columns in pixels, the scale of the drawing.
type Config = { orders :: Array Storey, superposition :: SuperpositionRule, numOfColumns :: Int, bayWidth :: Number, bayMeasure :: BayMeasure, rythm :: Rythm ColumnBeat, lowestLowerDiameter :: Number }

-- | Draw the stacked collonade centered on the surface.
draw :: forall m. Monad m => Renderer.Renderer m => Config -> Number -> Number -> m Unit
draw { orders, superposition, numOfColumns, bayWidth, bayMeasure, rythm, lowestLowerDiameter } width height =
  render $ placeCentered (point (width / 2.0) (height / 2.0)) $ StackedCollonade
    { orders
    , superposition
    , size: { row: 0, size: { measure: LowerDiameter, value: lowestLowerDiameter } }
    , numOfColumns
    , bayWidth: bayWidth * lowestLowerDiameter
    , bayMeasure
    , rythm
    }
