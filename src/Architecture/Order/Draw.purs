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

import Architecture.Order (Order, OrderKind(..), ionicVolutes, Pedestal, minuteFromColumnAndEntablatureHeight, minuteFromColumnHeight, minuteFromLowerDiameter, minuteFromUpperDiameter, sizeOf)
import Data.Array as Array
import Data.Default (def)
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
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
  let abacus = size order.capital.width
  case order.kind of
    Tuscan -> drawDoricCapital x shaftTop upperDiameter abacus capitalTop
    Doric -> drawDoricCapital x shaftTop upperDiameter abacus capitalTop
    Ionic -> drawIonicCapital x shaftTop abacus capitalTop
    Corynthian -> drawCorinthianCapital x shaftTop upperDiameter abacus capitalTop
    Composite -> drawCompositeCapital x shaftTop upperDiameter abacus capitalTop

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
  Renderer.drawRectangle (point (x - width / 2.0) top) width (bottom - top) (Just def) (Just def)

-- The capitals below are centered on x and span from bottom, the top of the shaft, up to top,
-- for a shaft of the given upper diameter and an abacus of the given width. Each part is drawn
-- over the parts behind it.

-- Tuscan or Doric capital, in thirds after Vitruvius (Book IV, ch. 3) and Palladio: the necking,
-- as wide as the top of the shaft; the echinus, a quarter round swelling out to nearly the width
-- of the abacus; and the abacus.
drawDoricCapital :: forall m. Monad m => Renderer.Renderer m => Number -> Number -> Number -> Number -> Number -> m Unit
drawDoricCapital x bottom upperDiameter abacus top = do
  let third = (bottom - top) / 3.0
  let neckingTop = bottom - third
  let echinusTop = neckingTop - third
  drawBox x bottom upperDiameter neckingTop
  drawOvolo x neckingTop upperDiameter echinusTop (abacus - (abacus - upperDiameter) / 5.0)
  drawBox x echinusTop abacus top

-- Ionic capital: the two volutes (see `ionicVolutes`), their eyes level with the top of the
-- shaft, and the abacus from top down to the tops of the volutes.
drawIonicCapital :: forall m. Monad m => Renderer.Renderer m => Number -> Number -> Number -> Number -> m Unit
drawIonicCapital x bottom abacus top = do
  let { inset, radius } = ionicVolutes abacus
  let voluteTop = bottom - radius
  drawVolutePair x bottom (abacus / 2.0 - inset) radius
  when (top < voluteTop) $ drawBox x voluteTop abacus top

-- Corinthian capital after Vitruvius (Book IV, ch. 1): the abacus a seventh of the capital and
-- the rest in thirds, two rows of acanthus leaves and the small volutes under the corners of
-- the abacus (see `drawLeaves`).
drawCorinthianCapital :: forall m. Monad m => Renderer.Renderer m => Number -> Number -> Number -> Number -> Number -> m Unit
drawCorinthianCapital x bottom upperDiameter abacus top = do
  let abacusBottom = top + (bottom - top) / 7.0
  let third = (bottom - abacusBottom) / 3.0
  let radius = third / 2.0
  drawLeaves x bottom upperDiameter abacus abacusBottom third
  drawVolutePair x (abacusBottom + radius) (abacus / 2.0 - radius) radius
  drawBox x abacusBottom abacus top

-- Composite capital: the Corinthian leaves under Ionic volutes, which reach from the abacus down
-- over the upper row of leaves.
drawCompositeCapital :: forall m. Monad m => Renderer.Renderer m => Number -> Number -> Number -> Number -> Number -> m Unit
drawCompositeCapital x bottom upperDiameter abacus top = do
  let abacusBottom = top + (bottom - top) / 7.0
  let third = (bottom - abacusBottom) / 3.0
  drawLeaves x bottom upperDiameter abacus abacusBottom third
  drawVolutePair x (abacusBottom + third) (abacus / 2.0 - third) third
  drawBox x abacusBottom abacus top

-- The bell, a trapezoid widening from the top of the shaft to the abacus, and two rows of
-- acanthus leaves as triangles fanning out with it. The lower row has three leaves; the upper
-- row, behind it, has a leaf between each two of them and a half leaf along each side of the bell.
drawLeaves :: forall m. Monad m => Renderer.Renderer m => Number -> Number -> Number -> Number -> Number -> Number -> m Unit
drawLeaves x bottom upperDiameter abacus abacusBottom row = do
  let widthAt y = upperDiameter + (abacus - upperDiameter) * (bottom - y) / (bottom - abacusBottom)
  let across width fraction = x - width / 2.0 + width * fraction
  let leaf tipY tip from to =
        Renderer.drawPolygon
          [ point (across upperDiameter (max 0.0 from)) bottom
          , point (across upperDiameter (min 1.0 to)) bottom
          , point (across (widthAt tipY) tip) tipY
          ]
          (Just def)
          (Just def)
  let upperTip = bottom - 2.0 * row
  let lowerTip = bottom - row
  drawTrapezoid x bottom upperDiameter abacusBottom abacus
  for_ [ 0.0, 1.0, 2.0, 3.0 ] \i -> leaf upperTip (i / 3.0) (i / 3.0 - 1.0 / 6.0) (i / 3.0 + 1.0 / 6.0)
  for_ [ 0.0, 1.0, 2.0 ] \i -> leaf lowerTip ((2.0 * i + 1.0) / 6.0) (i / 3.0) ((i + 1.0) / 3.0)

-- Two volutes as circles of the given radius, their centers at centerY and halfSpan either side
-- of x, over a rectangle joining them from their tops down to their centers.
drawVolutePair :: forall m. Monad m => Renderer.Renderer m => Number -> Number -> Number -> Number -> m Unit
drawVolutePair x centerY halfSpan radius = do
  drawBox x centerY (2.0 * halfSpan) (centerY - radius)
  Renderer.drawCircle (point (x - halfSpan) centerY) radius (Just def) (Just def)
  Renderer.drawCircle (point (x + halfSpan) centerY) radius (Just def) (Just def)

-- Quarter round centered on x, swelling from the bottom width up to the top width.
drawOvolo :: forall m. Renderer.Renderer m => Number -> Number -> Number -> Number -> Number -> m Unit
drawOvolo x bottom bottomWidth top topWidth =
  let projection = (topWidth - bottomWidth) / 2.0
      rise = bottom - top
      steps = 8
      profile side = map
        (\i ->
          let angle = Number.pi / 2.0 * Int.toNumber i / Int.toNumber steps
          in point (x + side * (bottomWidth / 2.0 + projection * Number.sin angle)) (top + rise * Number.cos angle))
        (Array.range 0 steps)
  in Renderer.drawPolygon (profile 1.0 <> Array.reverse (profile (-1.0))) (Just def) (Just def)

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
    (Just def)
