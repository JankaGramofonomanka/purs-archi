module Architecture
  ( Portico(..)
  , Collonade(..)
  , StackedCollonade(..)
  , Storey
  , RowSizeDeterminant
  , collonades
  , SizedOrder
  , Column(..)
  , Entablature(..)
  )
  where

import Prelude

import Anchor (class Sized, HorizontalAnchor(..), PointAnchor(..), RectangularAnchor(..), VerticalAnchor(..), drawAnchored, size)
import Architecture.Order (Order, Pedestal, PedestalMode(..), SizeDeterminant(..), minuteFromLowerDiameter, minuteFromSize, minuteFromUpperDiameter, pedestalHeight, sizeOf)
import Architecture.Order.Draw (drawColumn, drawEntablature, drawPedestal, drawSpanningEntablature, drawSpanningPedestal)
import Architecture.Rythm (Rythm, beatAt)
import Data.Array as Array
import Data.Foldable (foldl, sum)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Geometry (class HorizontalFigure, class RectangularFigure, class VerticalFigure)
import Ortogonal (class RenderHorizontal, class RenderOrtogonal, class RenderPoint, class RenderVertical, drawBottomCentered, drawLeftCentered, drawRightCentered, drawTopCentered)
import Renderer (Point(..), point, class Renderer)

type SizedOrder = { order :: Order, size :: SizeDeterminant }

newtype Column = Column SizedOrder
newtype Entablature = Entablature SizedOrder

-- | Columns of the same order in a row under a common entablature, standing on pedestals or a podium
-- | as `pedestal` says (and the order defines one). The row is a pulse of `numOfColumns` beats,
-- | `bayWidth` apart (centerline to centerline); a column stands on a beat where `rythm` is `true`,
-- | and the beat is left empty where it is `false`. The entablature (and a podium) span the whole pulse.
newtype Portico = Portico {
  order :: Order,
  size :: SizeDeterminant,
  numOfColumns :: Int,
  bayWidth :: Number,
  rythm :: Rythm Boolean,
  pedestal :: PedestalMode
}

-- | One row of columns of a `StackedCollonade`; drawn exactly like a `Portico`.
newtype Collonade = Collonade Portico

-- | A size determinant of the given row of a `StackedCollonade` (0 is the bottom row).
type RowSizeDeterminant = { row :: Int, size :: SizeDeterminant }

-- | An order of a `StackedCollonade` row and how its columns stand.
type Storey = { order :: Order, pedestal :: PedestalMode }

-- | Rows of columns stacked on each other, `orders` listed from bottom to top.
-- | All rows share the pulse and the `rythm` of the columns (see `Portico`); the lower diameter
-- | of each row's columns equals the upper diameter of the columns below, so every row has its own minute.
newtype StackedCollonade = StackedCollonade {
  orders :: Array Storey,
  size :: RowSizeDeterminant,
  numOfColumns :: Int,
  bayWidth :: Number,
  rythm :: Rythm Boolean
}

minuteOf :: SizedOrder -> Number
minuteOf sized = minuteFromSize sized.order sized.size

-- Column

instance Sized Column where
  size (Column sized) =
    let minute = minuteOf sized
    in { width: max (sizeOf sized.order.base.width minute) (sizeOf sized.order.capital.width minute)
       , height: sizeOf sized.order.column.height minute
       }

instance VerticalFigure Column
instance RectangularFigure Column

drawColumnFromTopLeft :: forall m. Monad m => Renderer m => Point -> Column -> m Unit
drawColumnFromTopLeft (Point topLeft) column@(Column sized) = do
  let { width, height } = size column
  drawColumn (point (topLeft.x + width / 2.0) (topLeft.y + height)) (minuteOf sized) sized.order

instance (Monad m, Renderer m) => RenderPoint m Column where
  drawCentered p = drawAnchored drawColumnFromTopLeft (ACenter p)

instance (Monad m, Renderer m) => RenderOrtogonal m Column where
  drawLeftCentered p = drawAnchored drawColumnFromTopLeft (ARight p)
  drawRightCentered p = drawAnchored drawColumnFromTopLeft (ALeft p)
  drawTopCentered p = drawAnchored drawColumnFromTopLeft (ABottom p)
  drawBottomCentered p = drawAnchored drawColumnFromTopLeft (ATop p)
  drawTopLeft p = drawAnchored drawColumnFromTopLeft (ABottomRight p)
  drawTopRight p = drawAnchored drawColumnFromTopLeft (ABottomLeft p)
  drawBottomLeft p = drawAnchored drawColumnFromTopLeft (ATopRight p)
  drawBottomRight p = drawAnchored drawColumnFromTopLeft (ATopLeft p)

instance (Monad m, Renderer m) => RenderVertical m Column where
  drawAbove = drawTopCentered
  drawBelow = drawBottomCentered

-- Entablature

instance Sized Entablature where
  size (Entablature sized) =
    let minute = minuteOf sized
    in { width: sizeOf sized.order.column.lowerDiameter minute
       , height: sizeOf sized.order.entablature.height minute
       }

instance HorizontalFigure Entablature
instance RectangularFigure Entablature

drawEntablatureFromTopLeft :: forall m. Monad m => Renderer m => Point -> Entablature -> m Unit
drawEntablatureFromTopLeft (Point topLeft) entablature@(Entablature sized) = do
  let { width, height } = size entablature
  drawEntablature (point (topLeft.x + width / 2.0) (topLeft.y + height)) (minuteOf sized) sized.order

instance (Monad m, Renderer m) => RenderPoint m Entablature where
  drawCentered p = drawAnchored drawEntablatureFromTopLeft (ACenter p)

instance (Monad m, Renderer m) => RenderOrtogonal m Entablature where
  drawLeftCentered p = drawAnchored drawEntablatureFromTopLeft (ARight p)
  drawRightCentered p = drawAnchored drawEntablatureFromTopLeft (ALeft p)
  drawTopCentered p = drawAnchored drawEntablatureFromTopLeft (ABottom p)
  drawBottomCentered p = drawAnchored drawEntablatureFromTopLeft (ATop p)
  drawTopLeft p = drawAnchored drawEntablatureFromTopLeft (ABottomRight p)
  drawTopRight p = drawAnchored drawEntablatureFromTopLeft (ABottomLeft p)
  drawBottomLeft p = drawAnchored drawEntablatureFromTopLeft (ATopRight p)
  drawBottomRight p = drawAnchored drawEntablatureFromTopLeft (ATopLeft p)

instance (Monad m, Renderer m) => RenderHorizontal m Entablature where
  drawLeft = drawLeftCentered
  drawRight = drawRightCentered

-- Portico

-- | Distance between the centerlines of the outermost columns.
porticoSpan :: Portico -> Number
porticoSpan (Portico portico) = Int.toNumber (max 0 (portico.numOfColumns - 1)) * portico.bayWidth

-- | The pedestal the portico's columns stand on, if any.
porticoPedestal :: Portico -> Maybe Pedestal
porticoPedestal (Portico portico) = case portico.pedestal of
  NoPedestal -> Nothing
  _ -> portico.order.pedestal

-- | How far the portico reaches beyond the centerlines of the outermost columns.
porticoOverhang :: Portico -> Number
porticoOverhang p@(Portico portico@{ order }) =
  let minute = minuteFromSize order portico.size
      size' proportion = sizeOf proportion minute
      pedestalOverhang = case porticoPedestal p of
        Nothing -> 0.0
        Just pedestal -> max (size' pedestal.base.width) (max (size' pedestal.die.width) (size' pedestal.cornice.width)) / 2.0
  in max pedestalOverhang
    $ max (size' order.base.width / 2.0)
    $ max (size' order.capital.width / 2.0)
    $ max (size' order.entablature.architrave.depth)
    $ max (size' order.entablature.frieze.depth) (size' order.entablature.cornice.depth)

instance Sized Portico where
  size p@(Portico portico) =
    let minute = minuteFromSize portico.order portico.size
    in { width: porticoSpan p + 2.0 * porticoOverhang p
       , height: sizeOf portico.order.height minute + sizeOf (pedestalHeight portico.pedestal portico.order) minute
       }

instance RectangularFigure Portico

drawPorticoFromTopLeft :: forall m. Monad m => Renderer m => Point -> Portico -> m Unit
drawPorticoFromTopLeft (Point topLeft) p@(Portico portico@{ order }) = do
  let minute = minuteFromSize order portico.size
  let { width, height } = size p
  let ground = topLeft.y + height
  let bottom = ground - sizeOf (pedestalHeight portico.pedestal order) minute
  let firstCenterline = topLeft.x + porticoOverhang p
  let centerline i = firstCenterline + Int.toNumber i * portico.bayWidth
  let forColumns draw =
        let go i = when (i < portico.numOfColumns) do
              when (beatAt i portico.rythm) $ draw (centerline i)
              go (i + 1)
        in go 0
  case porticoPedestal p of
    Nothing -> pure unit
    Just pedestal -> case portico.pedestal of
      Podium -> drawSpanningPedestal (point (topLeft.x + width / 2.0) ground) (porticoSpan p) minute pedestal
      _ -> forColumns \x -> drawPedestal (point x ground) minute pedestal
  forColumns \x -> drawColumn (point x bottom) minute order
  drawSpanningEntablature (point (topLeft.x + width / 2.0) (bottom - sizeOf order.column.height minute)) (porticoSpan p) minute order

instance (Monad m, Renderer m) => RenderPoint m Portico where
  drawCentered p = drawAnchored drawPorticoFromTopLeft (ACenter p)

instance (Monad m, Renderer m) => RenderOrtogonal m Portico where
  drawLeftCentered p = drawAnchored drawPorticoFromTopLeft (ARight p)
  drawRightCentered p = drawAnchored drawPorticoFromTopLeft (ALeft p)
  drawTopCentered p = drawAnchored drawPorticoFromTopLeft (ABottom p)
  drawBottomCentered p = drawAnchored drawPorticoFromTopLeft (ATop p)
  drawTopLeft p = drawAnchored drawPorticoFromTopLeft (ABottomRight p)
  drawTopRight p = drawAnchored drawPorticoFromTopLeft (ABottomLeft p)
  drawBottomLeft p = drawAnchored drawPorticoFromTopLeft (ATopRight p)
  drawBottomRight p = drawAnchored drawPorticoFromTopLeft (ATopLeft p)

-- Collonade

derive newtype instance Sized Collonade
instance RectangularFigure Collonade
derive newtype instance (Monad m, Renderer m) => RenderPoint m Collonade
derive newtype instance (Monad m, Renderer m) => RenderOrtogonal m Collonade

-- StackedCollonade

-- | Minutes of the rows, bottom to top. Empty if `size.row` is not a row of the stack.
rowMinutes :: StackedCollonade -> Array Number
rowMinutes (StackedCollonade { orders: storeys, size: { row, size } }) = case Array.index orders row of
  Nothing -> []
  Just anchor ->
    let anchorMinute = minuteFromSize anchor size
        start = Tuple anchor anchorMinute
        propagate next = map snd <<< Array.scanl (\(Tuple prev minute) order -> Tuple order (next prev minute order)) start
        above = propagate (\below minute order -> minuteFromLowerDiameter order (sizeOf below.column.upperDiameter minute))
          (Array.drop (row + 1) orders)
        below = propagate (\above' minute order -> minuteFromUpperDiameter order (sizeOf above'.column.lowerDiameter minute))
          (Array.reverse (Array.take row orders))
    in Array.reverse below <> [ anchorMinute ] <> above
  where
  orders = map _.order storeys

-- | Rows of the stack, bottom to top.
collonades :: StackedCollonade -> Array Collonade
collonades stack@(StackedCollonade { orders, numOfColumns, bayWidth, rythm }) =
  Array.zipWith
    (\{ order, pedestal } minute -> Collonade (Portico { order, size: Minute minute, numOfColumns, bayWidth, rythm, pedestal }))
    orders
    (rowMinutes stack)

-- | All rows share the span, so the widest row is the one with the largest overhang.
instance Sized StackedCollonade where
  size stack =
    let sizes = map size (collonades stack)
    in { width: foldl max 0.0 (map _.width sizes)
       , height: sum (map _.height sizes)
       }

instance RectangularFigure StackedCollonade

drawStackedCollonadeFromTopLeft :: forall m. Monad m => Renderer m => Point -> StackedCollonade -> m Unit
drawStackedCollonadeFromTopLeft (Point topLeft) stack = do
  let { width, height } = size stack
  let centerline = topLeft.x + width / 2.0
  let drawRow bottom collonade = do
        drawTopCentered (point centerline bottom) collonade
        pure (bottom - (size collonade).height)
  void $ Array.foldM drawRow (topLeft.y + height) (collonades stack)

instance (Monad m, Renderer m) => RenderPoint m StackedCollonade where
  drawCentered p = drawAnchored drawStackedCollonadeFromTopLeft (ACenter p)

instance (Monad m, Renderer m) => RenderOrtogonal m StackedCollonade where
  drawLeftCentered p = drawAnchored drawStackedCollonadeFromTopLeft (ARight p)
  drawRightCentered p = drawAnchored drawStackedCollonadeFromTopLeft (ALeft p)
  drawTopCentered p = drawAnchored drawStackedCollonadeFromTopLeft (ABottom p)
  drawBottomCentered p = drawAnchored drawStackedCollonadeFromTopLeft (ATop p)
  drawTopLeft p = drawAnchored drawStackedCollonadeFromTopLeft (ABottomRight p)
  drawTopRight p = drawAnchored drawStackedCollonadeFromTopLeft (ABottomLeft p)
  drawBottomLeft p = drawAnchored drawStackedCollonadeFromTopLeft (ATopRight p)
  drawBottomRight p = drawAnchored drawStackedCollonadeFromTopLeft (ATopLeft p)
