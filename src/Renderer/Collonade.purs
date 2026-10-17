module Renderer.Collonade (Config, draw, lowestLowerDiameter) where

import Prelude

import Anchor (placeCentered, render)
import Architecture (StackedCollonade(..))
import Architecture.Order (Order, SizeDeterminant(..))
import Renderer (point)
import Renderer as Renderer

-- | Orders listed from bottom to top; `bayWidth` is the distance between column centerlines,
-- | in lower diameters of the bottom row's columns.
type Config = { orders :: Array Order, numOfColumns :: Int, bayWidth :: Number }

-- | Lower diameter of the columns of the bottom row, in pixels.
lowestLowerDiameter :: Number
lowestLowerDiameter = 10.0

-- | Draw the stacked collonade centered on the surface.
draw :: forall m. Monad m => Renderer.Renderer m => Config -> Number -> Number -> m Unit
draw { orders, numOfColumns, bayWidth } width height =
  render $ placeCentered (point (width / 2.0) (height / 2.0)) $ StackedCollonade
    { orders
    , size: { row: 0, size: LowerDiameter lowestLowerDiameter }
    , numOfColumns
    , bayWidth: bayWidth * lowestLowerDiameter
    }
