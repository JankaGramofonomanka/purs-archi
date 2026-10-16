module Prototype where

import Prelude hiding (top, bottom)

import Anchor (bottom, bottomLeft, bottomRight, center, left, right, top, placeBottom, placeBottomCentered, placeBottomLeft, placeBottomRight, placeCentered, placeLeftCentered, placeRightCentered, render)
import Geometry (Circle(..), HLine(..), Rectangle(..), VLine(..))
import Renderer (point, class Renderer)

prototype :: forall m . Monad m => Number -> Number -> Renderer m => m Unit
prototype centerX centerY = do
  let square = Rectangle { width: 50.0, height: 50.0 }
  let rect = Rectangle { width: 50.0, height: 80.0 }
  let circ = Circle { radius: 25.0 }
  let vline5 = VLine { vLength: 50.0 }
  let vline8 = VLine { vLength: 80.0 }
  let hline5 = HLine { hLength: 50.0 }
  let hline15 = HLine { hLength: 150.0 }

  let pCenter = point centerX centerY

  sqMid <- pure $ placeBottomCentered pCenter square
  rectL <- pure $ placeBottomRight (bottomLeft sqMid) rect
  sqL1  <- pure $ placeBottomRight (bottomLeft rectL) square
  sqL2  <- pure $ placeBottomRight (bottomLeft sqL1) square
  rectR <- pure $ placeBottomLeft (bottomRight sqMid) rect
  sqR1  <- pure $ placeBottomLeft (bottomRight rectR) square
  sqR2  <- pure $ placeBottomLeft (bottomRight sqR1) square

  hlineMid <- pure $ placeCentered (center sqMid) hline15
  hlineL <- pure $ placeLeftCentered (left sqL2) hline5
  hLineR <- pure $ placeRightCentered (right sqR2) hline5

  vline8L <- pure $ placeBottom (bottom rectL) vline8
  vline8R <- pure $ placeBottom (bottom rectR) vline8
  vline5L <- pure $ placeBottom (bottom sqL2) vline5
  vline5R <- pure $ placeBottom (bottom sqR2) vline5

  circL1 <- pure $ placeCentered (top rectL) circ
  circL2 <- pure $ placeCentered (top sqL2) circ
  circR1 <- pure $ placeCentered (top rectR) circ
  circR2 <- pure $ placeCentered (top sqR2) circ

  render sqMid
  render rectL
  render sqL1
  render sqL2
  render rectR
  render sqR1
  render sqR2

  render hlineL
  render hlineMid
  render hLineR

  render vline8L
  render vline8R
  render vline5L
  render vline5R

  render circL1
  render circL2
  render circR1
  render circR2
