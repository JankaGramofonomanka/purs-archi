module Geometry where

import Prelude hiding (top, bottom)

import Anchor (class Anchor, class Sized, Placed, boxAt, figureBox)

import Data.Default (def)
import Data.Maybe (Maybe(..))
import Ortogonal (class RenderHorizontal, class RenderOrtogonal, class RenderPoint, class RenderVertical, drawBottomCentered, drawLeftCentered, drawRightCentered, drawTopCentered)
import Renderer (Point(..), point, class Renderer, drawCircle, drawLineSegment, drawRectangle)

newtype Rectangle = Rectangle { width :: Number, height :: Number }
newtype Circle = Circle { radius :: Number }
newtype VLine = VLine { vLength :: Number }
newtype HLine = HLine { hLength :: Number }

-- | Positions of a placed figure.
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

instance Sized Rectangle where
  size (Rectangle { width, height }) = { width, height }

instance Sized Circle where
  size (Circle { radius }) = { width: 2.0 * radius, height: 2.0 * radius }

instance Sized HLine where
  size (HLine { hLength }) = { width: hLength, height: 0.0 }

instance Sized VLine where
  size (VLine { vLength }) = { width: 0.0, height: vLength }

instance Anchor anc => PointLike (Placed anc Rectangle) where
  center = boxAt 0.5 0.5 <<< figureBox
instance Anchor anc => PointLike (Placed anc Circle) where
  center = boxAt 0.5 0.5 <<< figureBox
instance Anchor anc => PointLike (Placed anc HLine) where
  center = boxAt 0.5 0.5 <<< figureBox
instance Anchor anc => PointLike (Placed anc VLine) where
  center = boxAt 0.5 0.5 <<< figureBox

instance Anchor anc => Horizontal (Placed anc Rectangle) where
  left = boxAt 0.0 0.5 <<< figureBox
  right = boxAt 1.0 0.5 <<< figureBox
instance Anchor anc => Horizontal (Placed anc Circle) where
  left = boxAt 0.0 0.5 <<< figureBox
  right = boxAt 1.0 0.5 <<< figureBox
instance Anchor anc => Horizontal (Placed anc HLine) where
  left = boxAt 0.0 0.5 <<< figureBox
  right = boxAt 1.0 0.5 <<< figureBox

instance Anchor anc => Vertical (Placed anc Rectangle) where
  top = boxAt 0.5 0.0 <<< figureBox
  bottom = boxAt 0.5 1.0 <<< figureBox
instance Anchor anc => Vertical (Placed anc Circle) where
  top = boxAt 0.5 0.0 <<< figureBox
  bottom = boxAt 0.5 1.0 <<< figureBox
instance Anchor anc => Vertical (Placed anc VLine) where
  top = boxAt 0.5 0.0 <<< figureBox
  bottom = boxAt 0.5 1.0 <<< figureBox

instance Anchor anc => Rectangular (Placed anc Rectangle) where
  topLeft = boxAt 0.0 0.0 <<< figureBox
  topRight = boxAt 1.0 0.0 <<< figureBox
  bottomLeft = boxAt 0.0 1.0 <<< figureBox
  bottomRight = boxAt 1.0 1.0 <<< figureBox
instance Anchor anc => Rectangular (Placed anc Circle) where
  topLeft = boxAt 0.0 0.0 <<< figureBox
  topRight = boxAt 1.0 0.0 <<< figureBox
  bottomLeft = boxAt 0.0 1.0 <<< figureBox
  bottomRight = boxAt 1.0 1.0 <<< figureBox
