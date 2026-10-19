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
  , ColumnBeat(..)
  , BayMeasure(..)
  , minGroupSpacing
  )
  where

import Prelude

import Anchor (class Sized, HorizontalAnchor(..), PointAnchor(..), RectangularAnchor(..), VerticalAnchor(..), drawAnchored, size)
import Architecture.Measure (Measure(..), SizeDeterminant)
import Architecture.Order (Order, capitalWidth, Pedestal, PedestalMode(..), lowerMinute, minuteFromSize, pedestalHeight, sizeOf, upperMinute)
import Architecture.Superposition (SuperpositionRule)
import Architecture.Order.Draw (drawColumn, drawEntablature, drawSpanningEntablature, drawSpanningPedestal)
import Architecture.Rythm (Rythm, beatAt)
import Data.Array as Array
import Data.Foldable (foldl, for_, sum)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Geometry (class HorizontalFigure, class RectangularFigure, class VerticalFigure)
import Ortogonal (class RenderHorizontal, class RenderOrtogonal, class RenderPoint, class RenderVertical, drawBottomCentered, drawLeftCentered, drawRightCentered, drawTopCentered)
import Renderer (Point(..), point, class Renderer)

type SizedOrder = { order :: Order, size :: SizeDeterminant }

newtype Column = Column SizedOrder
newtype Entablature = Entablature SizedOrder

-- | The number of columns standing on a beat of a row of columns (0 or less leaves the beat empty).
-- | More than one column make a group: they stand symmetrically about the beat's centerline,
-- | `groupSpacing` apart, and share one pedestal.
newtype ColumnBeat = ColumnBeat Int

derive instance Eq ColumnBeat
derive newtype instance Show ColumnBeat

-- | Distance between the centerlines of the outermost columns of the group on the beat.
groupSpan :: Number -> ColumnBeat -> Number
groupSpan spacing (ColumnBeat n) = Int.toNumber (max 0 (n - 1)) * spacing

-- | What the bay width of a row of columns measures.
-- | `BetweenBeats`: the distance between the centerlines of neighbouring beats, so the beats keep
-- | an even pulse and groups of columns are centered on it.
-- | `BetweenGroups`: the distance between the last column of a group and the first column of the next
-- | (an empty beat counts as a group with no width), so wider groups stretch the pulse.
data BayMeasure = BetweenBeats | BetweenGroups

derive instance Eq BayMeasure

-- | Columns of the same order in a row under a common entablature, standing on pedestals or a podium
-- | as `pedestal` says (and the order defines one). The row is a sequence of `numOfColumns` beats,
-- | `bayWidth` apart as `bayMeasure` says, and `rythm` says what stands on each of them.
-- | The entablature (and a podium) span the whole row.
newtype Portico = Portico {
  order :: Order,
  size :: SizeDeterminant,
  numOfColumns :: Int,
  bayWidth :: Number,
  bayMeasure :: BayMeasure,
  rythm :: Rythm ColumnBeat,
  -- | Distance between the centerlines of neighbouring columns of a group; see `minGroupSpacing`.
  groupSpacing :: Number,
  pedestal :: PedestalMode
}

-- | One row of columns of a `StackedCollonade`; drawn exactly like a `Portico`.
newtype Collonade = Collonade Portico

-- | A size determinant of the given row of a `StackedCollonade` (0 is the bottom row).
type RowSizeDeterminant = { row :: Int, size :: SizeDeterminant }

-- | An order of a `StackedCollonade` row and how its columns stand.
type Storey = { order :: Order, pedestal :: PedestalMode }

-- | Rows of columns stacked on each other, `orders` listed from bottom to top.
-- | All rows share the beats and the `rythm` of the columns (see `Portico`); `superposition` relates
-- | the size of each row to the row below, so every row has its own minute.
newtype StackedCollonade = StackedCollonade {
  orders :: Array Storey,
  superposition :: SuperpositionRule,
  size :: RowSizeDeterminant,
  numOfColumns :: Int,
  bayWidth :: Number,
  bayMeasure :: BayMeasure,
  rythm :: Rythm ColumnBeat
}

minuteOf :: SizedOrder -> Number
minuteOf sized = minuteFromSize NoPedestal sized.order sized.size

-- Column

instance Sized Column where
  size (Column sized) =
    let minute = minuteOf sized
    in { width: max (sizeOf sized.order.base.width minute) (capitalWidth sized.order minute)
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

-- | The smallest distance between the centerlines of neighbouring columns of a group at which
-- | neither their bases nor their capitals overlap.
minGroupSpacing :: Order -> Number -> Number
minGroupSpacing order minute =
  max (sizeOf order.base.width minute) (capitalWidth order minute)

-- | The beats of the row, each with its centerline measured from the centerline of the leftmost column.
porticoBeats :: Portico -> Array { centerline :: Number, beat :: ColumnBeat }
porticoBeats (Portico portico) =
  let beats = map (\i -> beatAt i portico.rythm) (Array.range 0 (portico.numOfColumns - 1))
      halfSpan beat = groupSpan portico.groupSpacing beat / 2.0
      distance previous next = case portico.bayMeasure of
        BetweenBeats -> portico.bayWidth
        BetweenGroups -> halfSpan previous + portico.bayWidth + halfSpan next
      place placed beat = case Array.last placed of
        Nothing -> [ { centerline: halfSpan beat, beat } ]
        Just previous -> Array.snoc placed { centerline: previous.centerline + distance previous.beat beat, beat }
  in if portico.numOfColumns < 1 then [] else foldl place [] beats

-- | Distance between the centerlines of the outermost columns.
porticoSpan :: Portico -> Number
porticoSpan p@(Portico portico) = case Array.last (porticoBeats p) of
  Nothing -> 0.0
  Just { centerline, beat } -> centerline + groupSpan portico.groupSpacing beat / 2.0

-- | The pedestal the portico's columns stand on, if any.
porticoPedestal :: Portico -> Maybe Pedestal
porticoPedestal (Portico portico) = case portico.pedestal of
  NoPedestal -> Nothing
  _ -> portico.order.pedestal

-- | How far the portico reaches beyond the centerlines of the outermost columns.
porticoOverhang :: Portico -> Number
porticoOverhang p@(Portico portico@{ order }) =
  let minute = minuteFromSize portico.pedestal order portico.size
      size' proportion = sizeOf proportion minute
      pedestalOverhang = case porticoPedestal p of
        Nothing -> 0.0
        Just pedestal -> max (size' pedestal.base.width) (max (size' pedestal.die.width) (size' pedestal.cornice.width)) / 2.0
  in max pedestalOverhang
    $ max (size' order.base.width / 2.0)
    $ max (capitalWidth order minute / 2.0)
    $ max (size' order.entablature.architrave.depth)
    $ max (size' order.entablature.frieze.depth) (size' order.entablature.cornice.depth)

instance Sized Portico where
  size p@(Portico portico) =
    let minute = minuteFromSize portico.pedestal portico.order portico.size
    in { width: porticoSpan p + 2.0 * porticoOverhang p
       , height: sizeOf portico.order.height minute + sizeOf (pedestalHeight portico.pedestal portico.order) minute
       }

instance RectangularFigure Portico

drawPorticoFromTopLeft :: forall m. Monad m => Renderer m => Point -> Portico -> m Unit
drawPorticoFromTopLeft (Point topLeft) p@(Portico portico@{ order }) = do
  let minute = minuteFromSize portico.pedestal order portico.size
  let { width, height } = size p
  let ground = topLeft.y + height
  let bottom = ground - sizeOf (pedestalHeight portico.pedestal order) minute
  let left = topLeft.x + porticoOverhang p
  let forBeats draw = for_ (porticoBeats p) \{ centerline, beat } -> draw (left + centerline) beat
  case porticoPedestal p of
    Nothing -> pure unit
    Just pedestal -> case portico.pedestal of
      Podium -> drawSpanningPedestal (point (topLeft.x + width / 2.0) ground) (porticoSpan p) minute pedestal
      _ -> forBeats \x beat@(ColumnBeat n) ->
        when (n > 0) $ drawSpanningPedestal (point x ground) (groupSpan portico.groupSpacing beat) minute pedestal
  forBeats \x beat@(ColumnBeat n) -> do
    let leftmost = x - groupSpan portico.groupSpacing beat / 2.0
    when (n > 0) $ for_ (Array.range 0 (n - 1)) \j ->
      drawColumn (point (leftmost + Int.toNumber j * portico.groupSpacing) bottom) minute order
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
rowMinutes (StackedCollonade { orders: storeys, superposition, size: { row, size } }) = case Array.index storeys row of
  Nothing -> []
  Just anchor ->
    let anchorMinute = minuteFromSize anchor.pedestal anchor.order size
        -- Each step carries the storey and minute just reached, and the joint to cross next.
        propagate next nextJoint firstJoint = map (\{ minute } -> minute) <<< Array.scanl
          (\{ storey: prev, minute, joint } storey -> { storey, minute: next joint prev minute storey, joint: nextJoint joint })
          { storey: anchor, minute: anchorMinute, joint: firstJoint }
        above = propagate (upperMinute superposition) (_ + 1) row (Array.drop (row + 1) storeys)
        below = propagate (lowerMinute superposition) (_ - 1) (row - 1) (Array.reverse (Array.take row storeys))
    in Array.reverse below <> [ anchorMinute ] <> above

-- | Rows of the stack, bottom to top. Grouped columns of all rows are as far apart as the row
-- | that needs the most room requires, so the columns of each row stand on those below.
collonades :: StackedCollonade -> Array Collonade
collonades stack@(StackedCollonade { orders, numOfColumns, bayWidth, bayMeasure, rythm }) =
  let rows = Array.zip orders (rowMinutes stack)
      groupSpacing = foldl max 0.0 (map (\(Tuple { order } minute) -> minGroupSpacing order minute) rows)
  in rows <#> \(Tuple { order, pedestal } minute) ->
       Collonade (Portico { order, size: { measure: Minute, value: minute }, numOfColumns, bayWidth, bayMeasure, rythm, groupSpacing, pedestal })

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
