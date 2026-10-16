module Prototype where

import Prelude

import Data.Tuple (Tuple(..))
import Ortogonal (class RenderHorizontal, class RenderOrtogonal, class RenderPoint, class RenderVertical, drawAbove, drawBelow, drawBottomCentered, drawBottomLeft, drawBottomRight, drawCentered, drawLeft, drawLeftCentered, drawRight, drawRightCentered, drawTopCentered, drawTopLeft, drawTopRight)

import Renderer (Point(..), point, class Renderer)

class PointLike a where
  center :: a -> Point

class Vertical a where
  top :: a -> Point
  bottom :: a -> Point

class Horizontal a where
  left :: a -> Point
  right :: a -> Point

class Rectangular a where
  topLeft :: a -> Point
  topRight :: a -> Point
  bottomLeft :: a -> Point
  bottomRight :: a -> Point


newtype PointAnchor = ACenter Point
data VerticalAnchor = ATop Point | ABottom Point
data HorizontalAnchor = ALeft Point | ARight Point
data RectangularAnchor = ATopLeft Point | ATopRight Point | ABottomLeft Point | ABottomRight Point
data OrtogonalAnchor = P PointAnchor | V VerticalAnchor | H HorizontalAnchor | R RectangularAnchor

placeC :: forall a. PointAnchor -> a -> Tuple PointAnchor a
placeC anchor fig = Tuple anchor fig

placeV :: forall a. VerticalAnchor -> a -> Tuple VerticalAnchor a
placeV anchor fig = Tuple anchor fig

placeH :: forall a. HorizontalAnchor -> a -> Tuple HorizontalAnchor a
placeH anchor fig = Tuple anchor fig

placeR :: forall a. RectangularAnchor -> a -> Tuple RectangularAnchor a
placeR anchor fig = Tuple anchor fig

placeO :: forall a. OrtogonalAnchor -> a -> Tuple OrtogonalAnchor a
placeO anchor fig = Tuple anchor fig

placeLeftCentered :: forall a. Point -> a -> Tuple HorizontalAnchor a
placeLeftCentered p fig = placeH (ALeft p) fig
placeRightCentered :: forall a. Point -> a -> Tuple HorizontalAnchor a
placeRightCentered p fig = placeH (ARight p) fig
placeTopCentered :: forall a. Point -> a -> Tuple VerticalAnchor a
placeTopCentered p fig = placeV (ATop p) fig
placeBottomCentered :: forall a. Point -> a -> Tuple VerticalAnchor a
placeBottomCentered p fig = placeV (ABottom p) fig
placeTopLeft :: forall a. Point -> a -> Tuple RectangularAnchor a
placeTopLeft p fig = placeR (ATopLeft p) fig
placeTopRight :: forall a. Point -> a -> Tuple RectangularAnchor a
placeTopRight p fig = placeR (ATopRight p) fig
placeBottomLeft :: forall a. Point -> a -> Tuple RectangularAnchor a
placeBottomLeft p fig = placeR (ABottomLeft p) fig
placeBottomRight :: forall a. Point -> a -> Tuple RectangularAnchor a
placeBottomRight p fig = placeR (ABottomRight p) fig
placeTop :: forall a. Point -> a -> Tuple VerticalAnchor a
placeTop p fig = placeV (ATop p) fig
placeBottom :: forall a. Point -> a -> Tuple VerticalAnchor a
placeBottom p fig = placeV (ABottom p) fig
placeLeft :: forall a. Point -> a -> Tuple HorizontalAnchor a
placeLeft p fig = placeH (ALeft p) fig
placeRight :: forall a. Point -> a -> Tuple HorizontalAnchor a
placeRight p fig = placeH (ARight p) fig
placeCentered :: forall a. Point -> a -> Tuple PointAnchor a
placeCentered p fig = placeC (ACenter p) fig


newtype Rectangle = Rectangle { width :: Number, height :: Number }
newtype Circle = Circle { radius :: Number }
newtype VLine = VLine { vLength :: Number }
newtype HLine = HLine { hLength :: Number }

class Render m a where
  render :: a -> m Unit

instance RenderPoint m a => Render m (Tuple PointAnchor a) where
  render (Tuple (ACenter p) fig) = drawCentered p fig

instance RenderHorizontal m a => Render m (Tuple HorizontalAnchor a) where
  render (Tuple anchor fig) = case anchor of
    ALeft p -> drawRight p fig
    ARight p -> drawLeft p fig

instance RenderVertical m a => Render m (Tuple VerticalAnchor a) where
  render (Tuple anchor fig) = case anchor of
    ATop p -> drawBelow p fig
    ABottom p -> drawAbove p fig

instance RenderOrtogonal m a => Render m (Tuple RectangularAnchor a) where
  render (Tuple anchor fig) = case anchor of
    ATopLeft p -> drawBottomRight p fig
    ATopRight p -> drawBottomLeft p fig
    ABottomLeft p -> drawTopRight p fig
    ABottomRight p -> drawTopLeft p fig

instance RenderOrtogonal m a => Render m (Tuple OrtogonalAnchor a) where
  render (Tuple anchor fig) = case anchor of
    P (ACenter p) -> drawCentered p fig
    V (ATop p) -> drawBottomCentered p fig
    V (ABottom p) -> drawTopCentered p fig
    H (ALeft p) -> drawRightCentered p fig
    H (ARight p) -> drawLeftCentered p fig
    R r -> render (Tuple r fig)

instance PointLike (Tuple PointAnchor HLine) where
instance PointLike (Tuple PointAnchor VLine) where
instance PointLike (Tuple PointAnchor Rectangle) where
instance PointLike (Tuple PointAnchor Circle) where
instance Horizontal (Tuple HorizontalAnchor HLine) where
instance Horizontal (Tuple HorizontalAnchor Rectanlge) where
instance Horizontal (Tuple HorizontalAnchor Circle) where
instance Vertical (Tuple VerticalAnchor VLine) where
instance Vertical (Tuple VerticalAnchor Rectanlge) where
instance Vertical (Tuple VerticalAnchor Circle) where
instance Rectangular (Tuple RectangularAnchor Rectangle) where
instance Rectangular (Tuple RectangularAnchor Circle) where


{-
prototype :: forall m . Monad m => Renderer m => m Unit
prototype = do
  let square = Rectangle { width: 5.0, height: 5.0}
  let rect = Rectangle { width: 5.0, height: 8.0 }
  let circ = Circle { radius: 2.5 }
  let vline5 = VLine { vLength: 5.0 }
  let vline8 = VLine { vLength: 8.0 }
  let hline5 = HLine { hLength: 5.0 }
  let hline15 = HLine { hLength: 15.0 }

  let centerX = 0.0
  let centerY = 0.0
  let center = point centerX centerY

  sqMid <- pure $ placeBottomCentered center square
  rectL <- pure $ placeBottomRight (bottomLeft sqMid) rect
  sqL1  <- pure $ placeBottomRight (bottomLeft rectL) rect
  sqL2  <- pure $ placeBottomRight (bottomLeft sqL1) rect
  rectR <- pure $ placeBottomLeft (bottomRight sqMid) rect
  sqR1  <- pure $ placeBottomLeft (bottomRight rectR) rect
  sqR2  <- pure $ placeBottomLeft (bottomRight sqR1) rect

  hlineMid <- pure $ placeCentered center hline15
  hlineL <- pure $ placeLeftCentered (left sqL2) hline5
  hLineR <- pure $ placeRightCentered (right sqR2) hline5

  vline8L <- pure $ placeBottom (bottom rectL) vline8
  vline8R <- pure $ placeBottom (bottom rectR) vline8
  vline5L <- pure $ placeBottom (bottom sqL2) vline5
  vline5R <- pure $ placeBottom (bottom sqR2) vline5

  circL1 <- pure $ placeTop (top rectL) circ
  circL2 <- pure $ placeTop (top sqL2) circ
  circR1 <- pure $ placeTop (top rectR) circ
  circR2 <- pure $ placeTop (top sqR2) circ

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
-}