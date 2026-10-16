module Anchor where

import Prelude hiding (top, bottom)

import Data.Tuple (Tuple(..))
import Geometry (class Sized, Circle, HLine, Rectangle, VLine, size)
import Ortogonal (class RenderHorizontal, class RenderOrtogonal, class RenderPoint, class RenderVertical, drawAbove, drawBelow, drawBottomCentered, drawBottomLeft, drawBottomRight, drawCentered, drawLeft, drawLeftCentered, drawRight, drawRightCentered, drawTopCentered, drawTopLeft, drawTopRight)
import Renderer (Point(..), point)

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

shift :: Number -> Number -> Point -> Point
shift dx dy (Point p) = point (p.x + dx) (p.y + dy)

-- | Axis-aligned bounding box, given by its top-left corner and size.
type Box = { topLeft :: Point, width :: Number, height :: Number }

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

figureBox :: forall anc a. Anchor anc => Sized a => Tuple anc a -> Box
figureBox (Tuple anchor fig) =
  let { width, height } = size fig
  in { topLeft: anchorTopLeft width height anchor, width, height }

boxAt :: Number -> Number -> Box -> Point
boxAt fx fy b = shift (fx * b.width) (fy * b.height) b.topLeft

instance Anchor anc => PointLike (Tuple anc Rectangle) where
  center = boxAt 0.5 0.5 <<< figureBox
instance Anchor anc => PointLike (Tuple anc Circle) where
  center = boxAt 0.5 0.5 <<< figureBox
instance Anchor anc => PointLike (Tuple anc HLine) where
  center = boxAt 0.5 0.5 <<< figureBox
instance Anchor anc => PointLike (Tuple anc VLine) where
  center = boxAt 0.5 0.5 <<< figureBox

instance Anchor anc => Horizontal (Tuple anc Rectangle) where
  left = boxAt 0.0 0.5 <<< figureBox
  right = boxAt 1.0 0.5 <<< figureBox
instance Anchor anc => Horizontal (Tuple anc Circle) where
  left = boxAt 0.0 0.5 <<< figureBox
  right = boxAt 1.0 0.5 <<< figureBox
instance Anchor anc => Horizontal (Tuple anc HLine) where
  left = boxAt 0.0 0.5 <<< figureBox
  right = boxAt 1.0 0.5 <<< figureBox

instance Anchor anc => Vertical (Tuple anc Rectangle) where
  top = boxAt 0.5 0.0 <<< figureBox
  bottom = boxAt 0.5 1.0 <<< figureBox
instance Anchor anc => Vertical (Tuple anc Circle) where
  top = boxAt 0.5 0.0 <<< figureBox
  bottom = boxAt 0.5 1.0 <<< figureBox
instance Anchor anc => Vertical (Tuple anc VLine) where
  top = boxAt 0.5 0.0 <<< figureBox
  bottom = boxAt 0.5 1.0 <<< figureBox

instance Anchor anc => Rectangular (Tuple anc Rectangle) where
  topLeft = boxAt 0.0 0.0 <<< figureBox
  topRight = boxAt 1.0 0.0 <<< figureBox
  bottomLeft = boxAt 0.0 1.0 <<< figureBox
  bottomRight = boxAt 1.0 1.0 <<< figureBox
instance Anchor anc => Rectangular (Tuple anc Circle) where
  topLeft = boxAt 0.0 0.0 <<< figureBox
  topRight = boxAt 1.0 0.0 <<< figureBox
  bottomLeft = boxAt 0.0 1.0 <<< figureBox
  bottomRight = boxAt 1.0 1.0 <<< figureBox
