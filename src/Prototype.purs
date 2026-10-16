module Prototype where

import Prelude

import Data.Default (def)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Ortogonal (class RenderHorizontal, class RenderOrtogonal, class RenderPoint, class RenderVertical, drawAbove, drawBelow, drawBottomCentered, drawBottomLeft, drawBottomRight, drawCentered, drawLeft, drawLeftCentered, drawRight, drawRightCentered, drawTopCentered, drawTopLeft, drawTopRight)

import Renderer (Point(..), point, class Renderer, drawCircle, drawLineSegment, drawRectangle)

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

-- | Draw a rectangle given its top-left corner.
rectangleAt :: forall m. Renderer m => Number -> Number -> Number -> Number -> m Unit
rectangleAt x y width height = drawRectangle (point x y) width height (Just def) Nothing

-- | Draw a circle given the top-left corner of its bounding box.
circleAt :: forall m. Renderer m => Number -> Number -> Number -> m Unit
circleAt x y radius = drawCircle (point (x + radius) (y + radius)) radius (Just def) Nothing

hLineAt :: forall m. Renderer m => Number -> Number -> Number -> m Unit
hLineAt x y len = drawLineSegment (point x y) (point (x + len) y) def

vLineAt :: forall m. Renderer m => Number -> Number -> Number -> m Unit
vLineAt x y len = drawLineSegment (point x y) (point x (y + len)) def

instance Renderer m => RenderPoint m Rectangle where
  drawCentered (Point p) (Rectangle { width, height }) =
    rectangleAt (p.x - width / 2.0) (p.y - height / 2.0) width height

instance Renderer m => RenderOrtogonal m Rectangle where
  drawLeftCentered (Point p) (Rectangle { width, height }) =
    rectangleAt (p.x - width) (p.y - height / 2.0) width height
  drawRightCentered (Point p) (Rectangle { width, height }) =
    rectangleAt p.x (p.y - height / 2.0) width height
  drawTopCentered (Point p) (Rectangle { width, height }) =
    rectangleAt (p.x - width / 2.0) (p.y - height) width height
  drawBottomCentered (Point p) (Rectangle { width, height }) =
    rectangleAt (p.x - width / 2.0) p.y width height
  drawTopLeft (Point p) (Rectangle { width, height }) =
    rectangleAt (p.x - width) (p.y - height) width height
  drawTopRight (Point p) (Rectangle { width, height }) =
    rectangleAt p.x (p.y - height) width height
  drawBottomLeft (Point p) (Rectangle { width, height }) =
    rectangleAt (p.x - width) p.y width height
  drawBottomRight (Point p) (Rectangle { width, height }) =
    rectangleAt p.x p.y width height

-- | Centered on the other axis, matching the `Horizontal` instance for anchored rectangles.
instance Renderer m => RenderHorizontal m Rectangle where
  drawLeft = drawLeftCentered
  drawRight = drawRightCentered

-- | Centered on the other axis, matching the `Vertical` instance for anchored rectangles.
instance Renderer m => RenderVertical m Rectangle where
  drawAbove = drawTopCentered
  drawBelow = drawBottomCentered

instance Renderer m => RenderPoint m Circle where
  drawCentered p (Circle { radius }) = drawCircle p radius (Just def) Nothing

-- | Positions the circle's bounding box.
instance Renderer m => RenderOrtogonal m Circle where
  drawLeftCentered (Point p) (Circle { radius }) = circleAt (p.x - 2.0 * radius) (p.y - radius) radius
  drawRightCentered (Point p) (Circle { radius }) = circleAt p.x (p.y - radius) radius
  drawTopCentered (Point p) (Circle { radius }) = circleAt (p.x - radius) (p.y - 2.0 * radius) radius
  drawBottomCentered (Point p) (Circle { radius }) = circleAt (p.x - radius) p.y radius
  drawTopLeft (Point p) (Circle { radius }) = circleAt (p.x - 2.0 * radius) (p.y - 2.0 * radius) radius
  drawTopRight (Point p) (Circle { radius }) = circleAt p.x (p.y - 2.0 * radius) radius
  drawBottomLeft (Point p) (Circle { radius }) = circleAt (p.x - 2.0 * radius) p.y radius
  drawBottomRight (Point p) (Circle { radius }) = circleAt p.x p.y radius

instance Renderer m => RenderHorizontal m Circle where
  drawLeft = drawLeftCentered
  drawRight = drawRightCentered

instance Renderer m => RenderVertical m Circle where
  drawAbove = drawTopCentered
  drawBelow = drawBottomCentered

instance Renderer m => RenderPoint m HLine where
  drawCentered (Point p) (HLine { hLength }) = hLineAt (p.x - hLength / 2.0) p.y hLength

instance Renderer m => RenderHorizontal m HLine where
  drawLeft (Point p) (HLine { hLength }) = hLineAt (p.x - hLength) p.y hLength
  drawRight (Point p) (HLine { hLength }) = hLineAt p.x p.y hLength

instance Renderer m => RenderPoint m VLine where
  drawCentered (Point p) (VLine { vLength }) = vLineAt p.x (p.y - vLength / 2.0) vLength

instance Renderer m => RenderVertical m VLine where
  drawAbove (Point p) (VLine { vLength }) = vLineAt p.x (p.y - vLength) vLength
  drawBelow (Point p) (VLine { vLength }) = vLineAt p.x p.y vLength

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

-- | Left and right ends of a figure of the given width, anchored horizontally.
horizontalEnds :: Number -> HorizontalAnchor -> { left :: Point, right :: Point }
horizontalEnds width = case _ of
  ALeft p -> { left: p, right: shift width 0.0 p }
  ARight p -> { left: shift (-width) 0.0 p, right: p }

-- | Top and bottom ends of a figure of the given height, anchored vertically.
verticalEnds :: Number -> VerticalAnchor -> { top :: Point, bottom :: Point }
verticalEnds height = case _ of
  ATop p -> { top: p, bottom: shift 0.0 height p }
  ABottom p -> { top: shift 0.0 (-height) p, bottom: p }

-- | Top-left corner of a box of the given size, anchored at one of its corners.
boxTopLeft :: Number -> Number -> RectangularAnchor -> Point
boxTopLeft width height = case _ of
  ATopLeft p -> p
  ATopRight p -> shift (-width) 0.0 p
  ABottomLeft p -> shift 0.0 (-height) p
  ABottomRight p -> shift (-width) (-height) p

instance PointLike (Tuple PointAnchor HLine) where
  center (Tuple (ACenter p) _) = p
instance PointLike (Tuple PointAnchor VLine) where
  center (Tuple (ACenter p) _) = p
instance PointLike (Tuple PointAnchor Rectangle) where
  center (Tuple (ACenter p) _) = p
instance PointLike (Tuple PointAnchor Circle) where
  center (Tuple (ACenter p) _) = p

instance Horizontal (Tuple HorizontalAnchor HLine) where
  left (Tuple anchor (HLine { hLength })) = (horizontalEnds hLength anchor).left
  right (Tuple anchor (HLine { hLength })) = (horizontalEnds hLength anchor).right
instance Horizontal (Tuple HorizontalAnchor Rectangle) where
  left (Tuple anchor (Rectangle { width })) = (horizontalEnds width anchor).left
  right (Tuple anchor (Rectangle { width })) = (horizontalEnds width anchor).right
instance Horizontal (Tuple HorizontalAnchor Circle) where
  left (Tuple anchor (Circle { radius })) = (horizontalEnds (2.0 * radius) anchor).left
  right (Tuple anchor (Circle { radius })) = (horizontalEnds (2.0 * radius) anchor).right

instance Vertical (Tuple VerticalAnchor VLine) where
  top (Tuple anchor (VLine { vLength })) = (verticalEnds vLength anchor).top
  bottom (Tuple anchor (VLine { vLength })) = (verticalEnds vLength anchor).bottom
instance Vertical (Tuple VerticalAnchor Rectangle) where
  top (Tuple anchor (Rectangle { height })) = (verticalEnds height anchor).top
  bottom (Tuple anchor (Rectangle { height })) = (verticalEnds height anchor).bottom
instance Vertical (Tuple VerticalAnchor Circle) where
  top (Tuple anchor (Circle { radius })) = (verticalEnds (2.0 * radius) anchor).top
  bottom (Tuple anchor (Circle { radius })) = (verticalEnds (2.0 * radius) anchor).bottom

instance Rectangular (Tuple RectangularAnchor Rectangle) where
  topLeft (Tuple anchor (Rectangle { width, height })) = boxTopLeft width height anchor
  topRight (Tuple anchor (Rectangle { width, height })) = shift width 0.0 (boxTopLeft width height anchor)
  bottomLeft (Tuple anchor (Rectangle { width, height })) = shift 0.0 height (boxTopLeft width height anchor)
  bottomRight (Tuple anchor (Rectangle { width, height })) = shift width height (boxTopLeft width height anchor)
instance Rectangular (Tuple RectangularAnchor Circle) where
  topLeft (Tuple anchor (Circle { radius })) = boxTopLeft (2.0 * radius) (2.0 * radius) anchor
  topRight (Tuple anchor (Circle { radius })) = shift (2.0 * radius) 0.0 (boxTopLeft (2.0 * radius) (2.0 * radius) anchor)
  bottomLeft (Tuple anchor (Circle { radius })) = shift 0.0 (2.0 * radius) (boxTopLeft (2.0 * radius) (2.0 * radius) anchor)
  bottomRight (Tuple anchor (Circle { radius })) = shift (2.0 * radius) (2.0 * radius) (boxTopLeft (2.0 * radius) (2.0 * radius) anchor)

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