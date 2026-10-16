module Anchor where

import Prelude

import Data.Tuple (Tuple(..))
import Ortogonal (class RenderHorizontal, class RenderOrtogonal, class RenderPoint, class RenderVertical, drawAbove, drawBelow, drawBottomCentered, drawBottomLeft, drawBottomRight, drawCentered, drawLeft, drawLeftCentered, drawRight, drawRightCentered, drawTopCentered, drawTopLeft, drawTopRight)
import Renderer (Point(..), point)

newtype PointAnchor = ACenter Point
data VerticalAnchor = ATop Point | ABottom Point
data HorizontalAnchor = ALeft Point | ARight Point
data RectangularAnchor = ATopLeft Point | ATopRight Point | ABottomLeft Point | ABottomRight Point
data OrtogonalAnchor = P PointAnchor | V VerticalAnchor | H HorizontalAnchor | R RectangularAnchor

-- | A figure placed with an anchor.
newtype Placed anc a = Placed (Tuple anc a)

placeC :: forall a. PointAnchor -> a -> Placed PointAnchor a
placeC anchor fig = Placed (Tuple anchor fig)

placeV :: forall a. VerticalAnchor -> a -> Placed VerticalAnchor a
placeV anchor fig = Placed (Tuple anchor fig)

placeH :: forall a. HorizontalAnchor -> a -> Placed HorizontalAnchor a
placeH anchor fig = Placed (Tuple anchor fig)

placeR :: forall a. RectangularAnchor -> a -> Placed RectangularAnchor a
placeR anchor fig = Placed (Tuple anchor fig)

placeO :: forall a. OrtogonalAnchor -> a -> Placed OrtogonalAnchor a
placeO anchor fig = Placed (Tuple anchor fig)

placeLeftCentered :: forall a. Point -> a -> Placed HorizontalAnchor a
placeLeftCentered p fig = placeH (ALeft p) fig
placeRightCentered :: forall a. Point -> a -> Placed HorizontalAnchor a
placeRightCentered p fig = placeH (ARight p) fig
placeTopCentered :: forall a. Point -> a -> Placed VerticalAnchor a
placeTopCentered p fig = placeV (ATop p) fig
placeBottomCentered :: forall a. Point -> a -> Placed VerticalAnchor a
placeBottomCentered p fig = placeV (ABottom p) fig
placeTopLeft :: forall a. Point -> a -> Placed RectangularAnchor a
placeTopLeft p fig = placeR (ATopLeft p) fig
placeTopRight :: forall a. Point -> a -> Placed RectangularAnchor a
placeTopRight p fig = placeR (ATopRight p) fig
placeBottomLeft :: forall a. Point -> a -> Placed RectangularAnchor a
placeBottomLeft p fig = placeR (ABottomLeft p) fig
placeBottomRight :: forall a. Point -> a -> Placed RectangularAnchor a
placeBottomRight p fig = placeR (ABottomRight p) fig
placeTop :: forall a. Point -> a -> Placed VerticalAnchor a
placeTop p fig = placeV (ATop p) fig
placeBottom :: forall a. Point -> a -> Placed VerticalAnchor a
placeBottom p fig = placeV (ABottom p) fig
placeLeft :: forall a. Point -> a -> Placed HorizontalAnchor a
placeLeft p fig = placeH (ALeft p) fig
placeRight :: forall a. Point -> a -> Placed HorizontalAnchor a
placeRight p fig = placeH (ARight p) fig
placeCentered :: forall a. Point -> a -> Placed PointAnchor a
placeCentered p fig = placeC (ACenter p) fig

class Render m a where
  render :: a -> m Unit

instance RenderPoint m a => Render m (Placed PointAnchor a) where
  render (Placed (Tuple (ACenter p) fig)) = drawCentered p fig

instance RenderHorizontal m a => Render m (Placed HorizontalAnchor a) where
  render (Placed (Tuple anchor fig)) = case anchor of
    ALeft p -> drawRight p fig
    ARight p -> drawLeft p fig

instance RenderVertical m a => Render m (Placed VerticalAnchor a) where
  render (Placed (Tuple anchor fig)) = case anchor of
    ATop p -> drawBelow p fig
    ABottom p -> drawAbove p fig

instance RenderOrtogonal m a => Render m (Placed RectangularAnchor a) where
  render (Placed (Tuple anchor fig)) = case anchor of
    ATopLeft p -> drawBottomRight p fig
    ATopRight p -> drawBottomLeft p fig
    ABottomLeft p -> drawTopRight p fig
    ABottomRight p -> drawTopLeft p fig

instance RenderOrtogonal m a => Render m (Placed OrtogonalAnchor a) where
  render (Placed (Tuple anchor fig)) = case anchor of
    P (ACenter p) -> drawCentered p fig
    V (ATop p) -> drawBottomCentered p fig
    V (ABottom p) -> drawTopCentered p fig
    H (ALeft p) -> drawRightCentered p fig
    H (ARight p) -> drawLeftCentered p fig
    R r -> render (Placed (Tuple r fig))

shift :: Number -> Number -> Point -> Point
shift dx dy (Point p) = point (p.x + dx) (p.y + dy)

-- | Axis-aligned bounding box, given by its top-left corner and size.
type Box = { topLeft :: Point, width :: Number, height :: Number }

-- | Figures with a bounding box size.
class Sized a where
  size :: a -> { width :: Number, height :: Number }

-- | Anchors that locate a box of a given size. Edge anchors are centered on the other axis.
class Anchor anc where
  anchorTopLeft :: Number -> Number -> anc -> Point

instance Anchor PointAnchor where
  anchorTopLeft w h (ACenter p) = shift (-w / 2.0) (-h / 2.0) p

instance Anchor VerticalAnchor where
  anchorTopLeft w h = case _ of
    ATop p -> shift (-w / 2.0) 0.0 p
    ABottom p -> shift (-w / 2.0) (-h) p

instance Anchor HorizontalAnchor where
  anchorTopLeft w h = case _ of
    ALeft p -> shift 0.0 (-h / 2.0) p
    ARight p -> shift (-w) (-h / 2.0) p

instance Anchor RectangularAnchor where
  anchorTopLeft w h = case _ of
    ATopLeft p -> p
    ATopRight p -> shift (-w) 0.0 p
    ABottomLeft p -> shift 0.0 (-h) p
    ABottomRight p -> shift (-w) (-h) p

instance Anchor OrtogonalAnchor where
  anchorTopLeft w h = case _ of
    P a -> anchorTopLeft w h a
    V a -> anchorTopLeft w h a
    H a -> anchorTopLeft w h a
    R a -> anchorTopLeft w h a

figureBox :: forall anc a. Anchor anc => Sized a => Placed anc a -> Box
figureBox (Placed (Tuple anchor fig)) =
  let { width, height } = size fig
  in { topLeft: anchorTopLeft width height anchor, width, height }

boxAt :: Number -> Number -> Box -> Point
boxAt fx fy b = shift (fx * b.width) (fy * b.height) b.topLeft

-- | Draw a figure placed at an anchor, given a function drawing it from the top-left corner of its bounding box.
drawAnchored :: forall m anc a. Anchor anc => Sized a => (Point -> a -> m Unit) -> anc -> a -> m Unit
drawAnchored drawFromTopLeft anchor fig = drawFromTopLeft (figureBox (Placed (Tuple anchor fig))).topLeft fig
