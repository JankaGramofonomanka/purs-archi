module Architecture.Order.Draw
  ( drawColumn
  , drawEntablature
  , drawOrder
  , drawColumnFromOrderHeight
  , drawColumnFromColumnHeight
  , drawColumnFromLowerDiameter
  , drawColumnFromUpperDiameter
  , drawOrderFromOrderHeight
  , drawOrderFromColumnHeight
  , drawOrderFromLowerDiameter
  , drawOrderFromUpperDiameter
  ) where

import Prelude

import Architecture.Order (Order, OrderKind(..), minuteFromColumnAndEntablatureHeight, minuteFromColumnHeight, minuteFromLowerDiameter, minuteFromUpperDiameter, sizeOf)
import Data.Default (def)
import Data.Maybe (Maybe(..))
import Renderer (Point(..), point)
import Renderer as Renderer

-- | Draw the column of an order (base, shaft and capital) with the given size of a minute.
-- | The point is the center of the bottom edge of the column base.
drawColumn :: forall m. Monad m => Renderer.Renderer m => Point -> Number -> Order -> m Unit
drawColumn (Point bottom) minute order = do
  let size proportion = sizeOf proportion minute
  let x = bottom.x
  let baseTop = bottom.y - size order.base.height
  let shaftTop = baseTop - size order.column.shaftHeight
  let capitalTop = shaftTop - size order.capital.height
  let upperDiameter = size order.column.upperDiameter
  drawBox x bottom.y (size order.base.width) baseTop
  drawTrapezoid x baseTop (size order.column.lowerDiameter) shaftTop upperDiameter
  case order.kind of
    Ionic -> drawBox x shaftTop (size order.capital.width) capitalTop
    _ -> drawTrapezoid x shaftTop upperDiameter capitalTop (size order.capital.width)

-- | Draw the entablature of an order (architrave, frieze and cornice) with the given size of a minute.
-- | The point is the center of the bottom edge of the architrave.
drawEntablature :: forall m. Monad m => Renderer.Renderer m => Point -> Number -> Order -> m Unit
drawEntablature (Point bottom) minute order = do
  let size proportion = sizeOf proportion minute
  let x = bottom.x
  let width = size order.column.lowerDiameter
  let architraveTop = bottom.y - size order.entablature.architrave.height
  let friezeTop = architraveTop - size order.entablature.frieze.height
  let corniceTop = friezeTop - size order.entablature.cornice.height
  drawBox x bottom.y width architraveTop
  drawBox x architraveTop width friezeTop
  drawBox x friezeTop width corniceTop

-- | Draw the entire order, from the column base up to the cornice, with the given size of a minute.
-- | The point is the center of the bottom edge of the column base.
drawOrder :: forall m. Monad m => Renderer.Renderer m => Point -> Number -> Order -> m Unit
drawOrder (Point bottom) minute order = do
  drawColumn (Point bottom) minute order
  drawEntablature (point bottom.x (bottom.y - sizeOf order.column.height minute)) minute order

-- | Draw the column so that the entire order, including the entablature, has the given height.
drawColumnFromOrderHeight :: forall m. Monad m => Renderer.Renderer m => Point -> Number -> Order -> m Unit
drawColumnFromOrderHeight bottom height order =
  drawColumn bottom (minuteFromColumnAndEntablatureHeight order height) order

-- | Draw the column so that it has the given height.
drawColumnFromColumnHeight :: forall m. Monad m => Renderer.Renderer m => Point -> Number -> Order -> m Unit
drawColumnFromColumnHeight bottom height order =
  drawColumn bottom (minuteFromColumnHeight order height) order

-- | Draw the column so that its lower diameter is the given width.
drawColumnFromLowerDiameter :: forall m. Monad m => Renderer.Renderer m => Point -> Number -> Order -> m Unit
drawColumnFromLowerDiameter bottom diameter order =
  drawColumn bottom (minuteFromLowerDiameter order diameter) order

-- | Draw the column so that its upper diameter is the given width.
drawColumnFromUpperDiameter :: forall m. Monad m => Renderer.Renderer m => Point -> Number -> Order -> m Unit
drawColumnFromUpperDiameter bottom diameter order =
  drawColumn bottom (minuteFromUpperDiameter order diameter) order

-- | Draw the order so that, including the entablature, it has the given height.
drawOrderFromOrderHeight :: forall m. Monad m => Renderer.Renderer m => Point -> Number -> Order -> m Unit
drawOrderFromOrderHeight bottom height order =
  drawOrder bottom (minuteFromColumnAndEntablatureHeight order height) order

-- | Draw the order so that its column has the given height.
drawOrderFromColumnHeight :: forall m. Monad m => Renderer.Renderer m => Point -> Number -> Order -> m Unit
drawOrderFromColumnHeight bottom height order =
  drawOrder bottom (minuteFromColumnHeight order height) order

-- | Draw the order so that the lower diameter of its column is the given width.
drawOrderFromLowerDiameter :: forall m. Monad m => Renderer.Renderer m => Point -> Number -> Order -> m Unit
drawOrderFromLowerDiameter bottom diameter order =
  drawOrder bottom (minuteFromLowerDiameter order diameter) order

-- | Draw the order so that the upper diameter of its column is the given width.
drawOrderFromUpperDiameter :: forall m. Monad m => Renderer.Renderer m => Point -> Number -> Order -> m Unit
drawOrderFromUpperDiameter bottom diameter order =
  drawOrder bottom (minuteFromUpperDiameter order diameter) order

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
