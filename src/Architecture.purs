module Architecture
  ( Portico(..)
  , SizedOrder
  , Column(..)
  , Entablature(..)
  )
  where

import Prelude

import Anchor (class Sized, HorizontalAnchor(..), PointAnchor(..), RectangularAnchor(..), VerticalAnchor(..), drawAnchored, size)
import Architecture.Order (Order, SizeDeterminant, minuteFromSize, sizeOf)
import Architecture.Order.Draw (drawColumn, drawEntablature, drawSpanningEntablature)
import Data.Int as Int
import Geometry (class HorizontalFigure, class RectangularFigure, class VerticalFigure)
import Ortogonal (class RenderHorizontal, class RenderOrtogonal, class RenderPoint, class RenderVertical, drawBottomCentered, drawLeftCentered, drawRightCentered, drawTopCentered)
import Renderer (Point(..), point, class Renderer)

type SizedOrder = { order :: Order, size :: SizeDeterminant }

newtype Column = Column SizedOrder
newtype Entablature = Entablature SizedOrder

-- | Columns of the same order in a row, `bayWidth` apart (centerline to centerline), under a common entablature.
newtype Portico = Portico {
  order :: Order,
  size :: SizeDeterminant,
  numOfColumns :: Int,
  bayWidth :: Number
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

-- | How far the portico reaches beyond the centerlines of the outermost columns.
porticoOverhang :: Portico -> Number
porticoOverhang (Portico portico@{ order }) =
  let minute = minuteFromSize order portico.size
      size' proportion = sizeOf proportion minute
  in max (size' order.base.width / 2.0)
    $ max (size' order.capital.width / 2.0)
    $ max (size' order.entablature.architrave.depth)
    $ max (size' order.entablature.frieze.depth) (size' order.entablature.cornice.depth)

instance Sized Portico where
  size p@(Portico portico) =
    { width: porticoSpan p + 2.0 * porticoOverhang p
    , height: sizeOf portico.order.height (minuteFromSize portico.order portico.size)
    }

instance RectangularFigure Portico

drawPorticoFromTopLeft :: forall m. Monad m => Renderer m => Point -> Portico -> m Unit
drawPorticoFromTopLeft (Point topLeft) p@(Portico portico@{ order }) = do
  let minute = minuteFromSize order portico.size
  let { width, height } = size p
  let bottom = topLeft.y + height
  let firstCenterline = topLeft.x + porticoOverhang p
  let drawColumns i = when (i < portico.numOfColumns) do
        drawColumn (point (firstCenterline + Int.toNumber i * portico.bayWidth) bottom) minute order
        drawColumns (i + 1)
  drawColumns 0
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
