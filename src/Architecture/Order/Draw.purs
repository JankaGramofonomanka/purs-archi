module Architecture.Order.Draw
  ( drawColumn
  , drawEntablature
  , drawSpanningEntablature
  , drawPedestal
  , drawSpanningPedestal
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

import Architecture.Order (Order, OrderKind(..), Pedestal, minuteFromColumnAndEntablatureHeight, minuteFromColumnHeight, minuteFromLowerDiameter, minuteFromUpperDiameter, sizeOf)
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
    Ionic -> drawVolutes x shaftTop (size order.capital.width) capitalTop
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

-- | Draw an entablature resting on columns whose centerlines span the given width,
-- | with each element projecting its depth beyond the outermost centerlines.
-- | The point is the center of the bottom edge of the architrave.
drawSpanningEntablature :: forall m. Monad m => Renderer.Renderer m => Point -> Number -> Number -> Order -> m Unit
drawSpanningEntablature (Point bottom) span minute order = do
  let size proportion = sizeOf proportion minute
  let x = bottom.x
  let width element = span + 2.0 * size element.depth
  let architraveTop = bottom.y - size order.entablature.architrave.height
  let friezeTop = architraveTop - size order.entablature.frieze.height
  let corniceTop = friezeTop - size order.entablature.cornice.height
  drawBox x bottom.y (width order.entablature.architrave) architraveTop
  drawBox x architraveTop (width order.entablature.frieze) friezeTop
  drawBox x friezeTop (width order.entablature.cornice) corniceTop

-- | Draw a pedestal (base, die and cornice) with the given size of a minute.
-- | The point is the center of the bottom edge of the pedestal's base.
drawPedestal :: forall m. Monad m => Renderer.Renderer m => Point -> Number -> Pedestal -> m Unit
drawPedestal bottom = drawSpanningPedestal bottom 0.0

-- | Draw a continuous pedestal (a podium) under columns whose centerlines span the given
-- | width, with each element reaching half its width beyond the outermost centerlines.
-- | The point is the center of the bottom edge of the pedestal's base.
drawSpanningPedestal :: forall m. Monad m => Renderer.Renderer m => Point -> Number -> Number -> Pedestal -> m Unit
drawSpanningPedestal (Point bottom) span minute pedestal = do
  let size proportion = sizeOf proportion minute
  let x = bottom.x
  let width element = span + size element.width
  let baseTop = bottom.y - size pedestal.base.height
  let dieTop = baseTop - size pedestal.die.height
  let corniceTop = dieTop - size pedestal.cornice.height
  drawBox x bottom.y (width pedestal.base) baseTop
  drawBox x baseTop (width pedestal.die) dieTop
  drawBox x dieTop (width pedestal.cornice) corniceTop

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

-- Ionic capital centered on x, spanning from bottom up to top: two volutes as circles as high as
-- the capital at either end of its width, joined by a line along their tops and one through their centers.
drawVolutes :: forall m. Monad m => Renderer.Renderer m => Number -> Number -> Number -> Number -> m Unit
drawVolutes x bottom width top = do
  let radius = (bottom - top) / 2.0
  let centerY = top + radius
  let leftX = x - width / 2.0 + radius
  let rightX = x + width / 2.0 - radius
  Renderer.drawCircle (point leftX centerY) radius (Just def) Nothing
  Renderer.drawCircle (point rightX centerY) radius (Just def) Nothing
  Renderer.drawLineSegment (point leftX top) (point rightX top) def
  Renderer.drawLineSegment (point (leftX + radius) centerY) (point (rightX - radius) centerY) def

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
