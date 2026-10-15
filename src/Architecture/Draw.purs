module Architecture.Draw
  ( drawColumn
  , drawOrder
  ) where

import Prelude

import Architecture (Minutes, Order, OrderKind(..), minuteFromColumnHeight, sizeOf)
import Data.Default (def)
import Data.Maybe (Maybe(..))
import Renderer (Point(..), point)
import Renderer as Renderer

-- | Draw the column of an order (base, shaft and capital) with the given column height.
-- | The point is the center of the bottom edge of the column base.
drawColumn :: forall m. Monad m => Renderer.Renderer m => Point -> Number -> Order -> m Unit
drawColumn (Point bottom) height order = do
  let minutes = scale height order
  let x = bottom.x
  let baseTop = bottom.y - minutes order.base.height
  let shaftTop = baseTop - minutes order.column.shaftHeight
  let capitalTop = shaftTop - minutes order.capital.height
  let upperDiameter = minutes order.column.upperDiameter
  drawBox x bottom.y (minutes order.base.width) baseTop
  drawTrapezoid x baseTop (minutes order.column.lowerDiameter) shaftTop upperDiameter
  case order.kind of
    Ionic -> drawBox x shaftTop (minutes order.capital.width) capitalTop
    _ -> drawTrapezoid x shaftTop upperDiameter capitalTop (minutes order.capital.width)

-- | Draw the entire order, from the column base up to the cornice, with the given column height.
-- | The point is the center of the bottom edge of the column base.
drawOrder :: forall m. Monad m => Renderer.Renderer m => Point -> Number -> Order -> m Unit
drawOrder (Point bottom) height order = do
  drawColumn (Point bottom) height order
  let minutes = scale height order
  let x = bottom.x
  let width = minutes order.column.lowerDiameter
  let columnTop = bottom.y - minutes order.column.height
  let architraveTop = columnTop - minutes order.entablature.architrave.height
  let friezeTop = architraveTop - minutes order.entablature.frieze.height
  let corniceTop = friezeTop - minutes order.entablature.cornice.height
  drawBox x columnTop width architraveTop
  drawBox x architraveTop width friezeTop
  drawBox x friezeTop width corniceTop

-- Rectangle centered on x, spanning from bottom up to top.
drawBox :: forall m. Renderer.Renderer m => Number -> Number -> Number -> Number -> m Unit
drawBox x bottom width top =
  Renderer.drawRectangle (point (x - width / 2.0) top) width (bottom - top) (Just def) Nothing

-- Trapezoid centered on x, with horizontal bottom and top edges of the given widths.
drawTrapezoid :: forall m. Renderer.Renderer m => Number -> Number -> Number -> Number -> Number -> m Unit
drawTrapezoid x bottom bottomWidth top topWidth =
  Renderer.drawPolygon
    [ point (x - bottomWidth / 2.0) bottom
    , point (x + bottomWidth / 2.0) bottom
    , point (x + topWidth / 2.0) top
    , point (x - topWidth / 2.0) top
    ]
    (Just def)
    Nothing

-- Converts minutes to drawing units, so that the column is the given height.
scale :: Number -> Order -> Minutes -> Number
scale height order proportion =
  sizeOf proportion (minuteFromColumnHeight order height)
