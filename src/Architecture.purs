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
  , BayFill(..)
  , WindowSize
  , minGroupSpacing
  )
  where

import Prelude

import Anchor (class Sized, HorizontalAnchor(..), PointAnchor(..), RectangularAnchor(..), VerticalAnchor(..), drawAnchored, size)
import Architecture.Measure (CollonadeMeasure, Measure(..), SizeDeterminant)
import Architecture.Order (Arcade, Minutes(..), Order, Pedestal, PedestalMode(..), arcadeOf, arcadePlinthHeight, capitalWidth, lowerMinute, minuteFromSize, pedestalHeight, sizeOf, upperMinute)
import Architecture.Order as Order
import Architecture.Order.Draw (drawColumn, drawEntablature, drawSpanningEntablature, drawSpanningPedestal)
import Architecture.Rythm (Rythm, beatAt)
import Architecture.Superposition (SuperpositionRule)
import Data.Array as Array
import Data.Default (def)
import Data.Foldable (foldl, for_, sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Number (cos, pi, pow, sin)
import Data.Tuple (Tuple(..))
import Geometry (class HorizontalFigure, class RectangularFigure, class VerticalFigure)
import Ortogonal (class RenderHorizontal, class RenderOrtogonal, class RenderPoint, class RenderVertical, drawBottomCentered, drawLeftCentered, drawRightCentered, drawTopCentered)
import Proportion (class Measurable, measure)
import Renderer (Color(..), Fill(..), Opaqueness(..), Point(..), point, class Renderer, drawPolygon, drawRectangle)

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

-- | What fills a bay of a row of columns: the space between the columns of neighbouring beats.
-- | A beat with no column does not merge the bays on its sides; each keeps its own fill.
data BayFill = Wall | Window

derive instance Eq BayFill

-- | The opening of the windows of a row, without their frame.
type WindowSize = { width :: Number, height :: Number }

-- | Columns of the same order in a row under a common entablature, standing on pedestals or a podium
-- | as `pedestal` says (and the order defines one). The row is a sequence of `numOfColumns` beats,
-- | `bayWidth` apart as `bayMeasure` says, and `rythm` says what stands on each of them. `bays` says
-- | what fills each bay (the first bay lies between the first and the second beat); every `Window`
-- | has an opening of the size `window`, centered between the facing columns, its frame resting on
-- | the line of the pedestals' tops (see `sillHeight`). The entablature (and a podium) span the whole row.
-- | If `arcade` is set and the order has an arcade for columns standing as `pedestal` says, every
-- | bay is decorated with a blind arch (see `drawArch`), with its window, if any, set in it; columns
-- | without pedestals then stand on the order's arcade plinth, if it has one (see `porticoRaise`).
newtype Portico = Portico {
  order :: Order,
  size :: SizeDeterminant,
  numOfColumns :: Int,
  bayWidth :: Number,
  bayMeasure :: BayMeasure,
  rythm :: Rythm ColumnBeat,
  -- | Distance between the centerlines of neighbouring columns of a group; see `minGroupSpacing`.
  groupSpacing :: Number,
  pedestal :: PedestalMode,
  bays :: Rythm BayFill,
  window :: WindowSize,
  arcade :: Boolean
}

-- | One row of columns of a `StackedCollonade`; drawn exactly like a `Portico`.
newtype Collonade = Collonade Portico

instance Measurable Collonade CollonadeMeasure Minutes where
  measure (Collonade (Portico collonade)) msr = Order.storeyMeasureOf msr collonade.pedestal collonade.order

-- | A size determinant of the given row of a `StackedCollonade` (0 is the bottom row).
type RowSizeDeterminant = { row :: Int, size :: SizeDeterminant }

-- | An order of a `StackedCollonade` row, how its columns stand and whether they frame an arcade.
type Storey = { order :: Order, pedestal :: PedestalMode, arcade :: Boolean }

-- | Rows of columns stacked on each other, `orders` listed from bottom to top.
-- | All rows share the beats and the `rythm` of the columns (see `Portico`), and the `bays` too, so the
-- | windows of the rows stand over each other; `superposition` relates the size of each row to the row
-- | below, so every row has its own minute. All windows are `windowWidth` wide; see `windowSizes`.
newtype StackedCollonade = StackedCollonade {
  orders :: Array Storey,
  superposition :: SuperpositionRule,
  size :: RowSizeDeterminant,
  numOfColumns :: Int,
  bayWidth :: Number,
  bayMeasure :: BayMeasure,
  rythm :: Rythm ColumnBeat,
  bays :: Rythm BayFill,
  windowWidth :: Number
}

minuteOf :: SizedOrder -> Number
minuteOf sized = minuteFromSize sized.order sized.size

-- Column

instance Sized Column where
  size (Column sized) =
    let minute = minuteOf sized
    in { width: max (sizeOf (unwrap sized.order).base.width minute) (capitalWidth sized.order minute)
       , height: sizeOf (unwrap sized.order).column.height minute
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
    in { width: sizeOf (unwrap sized.order).column.lowerDiameter minute
       , height: sizeOf (unwrap sized.order).entablature.height minute
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
  max (sizeOf (unwrap order).base.width minute) (capitalWidth order minute)

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

-- | The bays of the row, each with the centerlines of the columns facing each other across it (the
-- | beat's centerline if no column stands on it) measured as in `porticoBeats`, and what fills it.
porticoBays :: Portico -> Array { left :: Number, right :: Number, fill :: BayFill }
porticoBays p@(Portico portico) =
  let beats = porticoBeats p
      edge side { centerline, beat } = centerline + side * groupSpan portico.groupSpacing beat / 2.0
  in Array.mapWithIndex
       (\i (Tuple l r) -> { left: edge 1.0 l, right: edge (-1.0) r, fill: beatAt i portico.bays })
       (Array.zip beats (Array.drop 1 beats))

-- | Width of the frame around a window's opening: Palladio makes the jambs from a sixth to a fifth
-- | of the opening's width; the sixth is used.
windowFrame :: WindowSize -> Number
windowFrame { width } = width / 6.0

-- | Height of the window sills above the bottom of the row: the top of the order's pedestal, whether
-- | the columns stand on pedestals or not (Chambers lines the sills up with the pedestals). The bottom
-- | of the row if the order has no pedestal.
sillHeight :: Order -> Number -> Number
sillHeight order minute = maybe 0.0 (\pedestal -> sizeOf pedestal.height minute) (unwrap order).pedestal

-- | How high the row's columns stand above the bottom of the row: on their pedestals or podium, or
-- | on the plinth of an arcade without pedestals.
porticoRaise :: Portico -> Number -> Number
porticoRaise (Portico { order, pedestal, arcade }) minute =
  sizeOf (pedestalHeight pedestal order) minute + sizeOf (arcadePlinthHeight pedestal arcade order) minute

-- | Height of the bottom of the capitals above the bottom of the row.
capitalBottom :: Portico -> Number -> Number
capitalBottom p@(Portico { order }) minute =
  porticoRaise p minute + sizeOf (unwrap order).base.height minute + sizeOf (unwrap order).column.shaftHeight minute

-- | The highest opening of the given width the row allows, its frame between the sill and the capitals.
maxWindowHeight :: Portico -> Number -> Number
maxWindowHeight p@(Portico { order, size }) width =
  let minute = minuteFromSize order size
  in capitalBottom p minute - sillHeight order minute - 2.0 * windowFrame { width, height: 0.0 }

-- | The pedestal the portico's columns stand on, if any.
porticoPedestal :: Portico -> Maybe Pedestal
porticoPedestal (Portico portico) = case portico.pedestal of
  NoPedestal -> Nothing
  _ -> (unwrap portico.order).pedestal

-- | How far the portico reaches beyond the centerlines of the outermost columns.
porticoOverhang :: Portico -> Number
porticoOverhang p@(Portico portico@{ order }) =
  let minute = minuteFromSize order portico.size
      size' proportion = sizeOf proportion minute
      pedestalOverhang = case porticoPedestal p of
        Nothing -> 0.0
        Just pedestal -> max (size' pedestal.base.width) (max (size' pedestal.die.width) (size' pedestal.cornice.width)) / 2.0
  in max pedestalOverhang
    $ max (size' (unwrap order).base.width / 2.0)
    $ max (capitalWidth order minute / 2.0)
    $ max (size' (unwrap order).entablature.architrave.depth)
    $ max (size' (unwrap order).entablature.frieze.depth) (size' (unwrap order).entablature.cornice.depth)

instance Sized Portico where
  size p@(Portico portico) =
    let minute = minuteFromSize portico.order portico.size
    in { width: porticoSpan p + 2.0 * porticoOverhang p
       , height: sizeOf (unwrap portico.order).height minute + porticoRaise p minute
       }

instance RectangularFigure Portico

drawPorticoFromTopLeft :: forall m. Monad m => Renderer m => Point -> Portico -> m Unit
drawPorticoFromTopLeft (Point topLeft) p@(Portico portico@{ order }) = do
  let minute = minuteFromSize order portico.size
  let { width, height } = size p
  let ground = topLeft.y + height
  let bottom = ground - porticoRaise p minute
  let left = topLeft.x + porticoOverhang p
  let forBeats draw = for_ (porticoBeats p) \{ centerline, beat } -> draw (left + centerline) beat
  let frame = windowFrame portico.window
  let sill = ground - sillHeight order minute
  let arches = if portico.arcade then arcadeOf portico.pedestal order else Nothing
  for_ arches \arcade -> for_ (porticoBays p) \bay ->
    drawArch { left: left + bay.left, right: left + bay.right } ground minute arcade
  when (portico.window.width > 0.0 && portico.window.height > 0.0) do
    for_ (porticoBays p) \bay -> when (bay.fill == Window) do
      let x = left + (bay.left + bay.right) / 2.0
      let rectangle w h bottom' fill = drawRectangle (point (x - w / 2.0) (bottom' - h)) w h (Just def) (Just fill)
      rectangle (portico.window.width + 2.0 * frame) (portico.window.height + 2.0 * frame) sill def
      rectangle portico.window.width portico.window.height (sill - frame) windowOpening
  case porticoPedestal p of
    Nothing -> pure unit
    Just pedestal -> case portico.pedestal of
      Podium -> drawSpanningPedestal (point (topLeft.x + width / 2.0) ground) (porticoSpan p) minute pedestal
      _ -> forBeats \x beat@(ColumnBeat n) ->
        when (n > 0) $ drawSpanningPedestal (point x ground) (groupSpan portico.groupSpacing beat) minute pedestal
  -- The plinths of an arcade without pedestals: plain blocks as wide as the plinths of the bases.
  let plinth = sizeOf (arcadePlinthHeight portico.pedestal portico.arcade order) minute
  when (plinth > 0.0) $ forBeats \x beat@(ColumnBeat n) -> when (n > 0) do
    let w = groupSpan portico.groupSpacing beat + sizeOf (unwrap order).base.width minute
    drawRectangle (point (x - w / 2.0) (ground - plinth)) w plinth (Just def) (Just def)
  forBeats \x beat@(ColumnBeat n) -> do
    let leftmost = x - groupSpan portico.groupSpacing beat / 2.0
    when (n > 0) $ for_ (Array.range 0 (n - 1)) \j ->
      drawColumn (point (leftmost + Int.toNumber j * portico.groupSpacing) bottom) minute order
  drawSpanningEntablature (point (topLeft.x + width / 2.0) (bottom - sizeOf (unwrap order).column.height minute)) (porticoSpan p) minute order

-- | Draw an arch of the arcade centered between the facing columns at `left` and `right`, standing on
-- | `ground`: an impost across the bay at the springing of the arch, a ninth of the opening high
-- | (Chambers' smallest impost), the archivolt as wide as the arcade says, and the opening.
-- | The arch is blind: its opening is walled up. The columns, pedestals and windows drawn over it
-- | cover the impost on the piers' axes and whatever of the arch they stand in front of.
drawArch :: forall m. Monad m => Renderer m => { left :: Number, right :: Number } -> Number -> Number -> Arcade -> m Unit
drawArch { left, right } ground minute arcade = do
  let x = (left + right) / 2.0
  let radius = sizeOf arcade.openingWidth minute / 2.0
  let springing = ground - sizeOf arcade.openingHeight minute + radius
  let archivolt = sizeOf arcade.archivoltWidth minute
  let impost = 2.0 * radius / 9.0
  let segments = 48
  let semicircle r = Array.range 0 segments <#> \i ->
        let angle = pi * Int.toNumber i / Int.toNumber segments
        in point (x - r * cos angle) (springing - r * sin angle)
  drawRectangle (point left springing) (right - left) impost (Just def) (Just def)
  drawPolygon (semicircle (radius + archivolt) <> Array.reverse (semicircle radius)) (Just def) (Just def)
  drawPolygon ([ point (x - radius) ground ] <> semicircle radius <> [ point (x + radius) ground ]) (Just def) (Just def)

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
    let rows = storeys <#> \{ order, pedestal } -> { order, pedestal }
        anchorMinute = minuteFromSize anchor.order size
        -- Each step carries the storey and minute just reached, and the joint to cross next.
        propagate next nextJoint firstJoint first = map (\{ minute } -> minute) <<< Array.scanl
          (\{ storey: prev, minute, joint } storey -> { storey, minute: next joint prev minute storey, joint: nextJoint joint })
          { storey: first, minute: anchorMinute, joint: firstJoint }
        start = { order: anchor.order, pedestal: anchor.pedestal }
        above = propagate (upperMinute superposition) (_ + 1) row start (Array.drop (row + 1) rows)
        below = propagate (lowerMinute superposition) (_ - 1) (row - 1) start (Array.reverse (Array.take row rows))
    in Array.reverse below <> [ anchorMinute ] <> above

-- | Rows of the stack, bottom to top. Grouped columns of all rows are as far apart as the row
-- | that needs the most room requires, so the columns of each row stand on those below.
collonades :: StackedCollonade -> Array Collonade
collonades stack@(StackedCollonade { orders, numOfColumns, bayWidth, bayMeasure, rythm, bays, windowWidth }) =
  let rows = Array.zip orders (rowMinutes stack)
      groupSpacing = foldl max 0.0 (map (\(Tuple { order } minute) -> minGroupSpacing order minute) rows)
      porticos = rows <#> \(Tuple { order, pedestal, arcade } minute) ->
        Portico { order, size: { measure: Minute, value: minute }, numOfColumns, bayWidth, bayMeasure, rythm, groupSpacing, pedestal, bays, window: { width: 0.0, height: 0.0 }, arcade }
  in Array.zipWith (\(Portico portico) window -> Collonade (Portico portico { window = window })) porticos (windowSizes windowWidth porticos)

-- | The windows of the rows, bottom to top, all of the given width (Palladio, Chambers: the same in
-- | every storey). The bottom row's windows are two widths and a sixth high and those of each row
-- | above a sixth lower than those below (Palladio), unless the row is too low for them (see `maxWindowHeight`).
windowSizes :: Number -> Array Portico -> Array WindowSize
windowSizes width = Array.mapWithIndex \row portico ->
  { width, height: max 0.0 (min (13.0 / 6.0 * width * pow (5.0 / 6.0) (Int.toNumber row)) (maxWindowHeight portico width)) }

-- | All rows share the span, so the widest row is the one with the largest overhang.
instance Sized StackedCollonade where
  size stack =
    let sizes = map size (collonades stack)
    in { width: foldl max 0.0 (map _.width sizes)
       , height: sum (map _.height sizes)
       }

instance RectangularFigure StackedCollonade

-- | The fill of a window's opening.
windowOpening :: Fill
windowOpening = Fill { opaqueness: Opaqueness 1.0, color: RGB { r: 190.0, g: 190.0, b: 190.0 } }

-- | Draws the rows on a white wall that reaches from the ground to the top of the stack, between
-- | the centerlines of the outermost columns.
drawStackedCollonadeFromTopLeft :: forall m. Monad m => Renderer m => Point -> StackedCollonade -> m Unit
drawStackedCollonadeFromTopLeft (Point topLeft) stack = do
  let { width, height } = size stack
  let centerline = topLeft.x + width / 2.0
  let span = maybe 0.0 (\(Collonade portico) -> porticoSpan portico) (Array.head (collonades stack))
  drawRectangle (point (centerline - span / 2.0) topLeft.y) span height (Just def) (Just def)
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
