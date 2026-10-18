module Renderer.Collonade (Config, draw, lowestLowerDiameter) where

import Prelude

import Anchor (placeCentered, render)
import Architecture (BayMeasure, ColumnBeat, StackedCollonade(..), Storey)
import Architecture.Order (SizeDeterminant(..))
import Architecture.Rythm (Rythm)
import Renderer (point)
import Renderer as Renderer

-- | Storeys (orders and how their columns stand) listed from bottom to top; `bayWidth` is the distance between column centerlines,
-- | in lower diameters of the bottom row's columns, measured as `bayMeasure` says; `rythm` tells how many columns
-- | stand on each of the `numOfColumns` beats.
type Config = { orders :: Array Storey, numOfColumns :: Int, bayWidth :: Number, bayMeasure :: BayMeasure, rythm :: Rythm ColumnBeat }

-- | Lower diameter of the columns of the bottom row, in pixels.
lowestLowerDiameter :: Number
lowestLowerDiameter = 10.0

-- | Draw the stacked collonade centered on the surface.
draw :: forall m. Monad m => Renderer.Renderer m => Config -> Number -> Number -> m Unit
draw { orders, numOfColumns, bayWidth, bayMeasure, rythm } width height =
  render $ placeCentered (point (width / 2.0) (height / 2.0)) $ StackedCollonade
    { orders
    , size: { row: 0, size: LowerDiameter lowestLowerDiameter }
    , numOfColumns
    , bayWidth: bayWidth * lowestLowerDiameter
    , bayMeasure
    , rythm
    }
