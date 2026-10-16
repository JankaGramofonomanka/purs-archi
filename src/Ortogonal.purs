module Ortogonal where

import Prelude

import Data.Newtype (class Newtype)
import Renderer (Point)

class RenderPoint m a <= RenderOrtogonal m a where
  -- | Draw a figure to the left of the given point, centered vertically.
  drawLeftCentered :: Point -> a -> m Unit

  -- | Draw a figure to the right of the given point, centered vertically.
  drawRightCentered :: Point -> a -> m Unit

  -- | Draw a figure above the given point, centered horizontally.
  drawTopCentered :: Point -> a -> m Unit

  -- | Draw a figure below the given point, centered horizontally.
  drawBottomCentered :: Point -> a -> m Unit

  -- | Draw a figure above and to the left of the given point.
  drawTopLeft :: Point -> a -> m Unit

  -- | Draw a figure above and to the right of the given point.
  drawTopRight :: Point -> a -> m Unit

  -- | Draw a figure below and to the left of the given point.
  drawBottomLeft :: Point -> a -> m Unit

  -- | Draw a figure below and to the right of the given point.
  drawBottomRight :: Point -> a -> m Unit

class RenderPoint m a <= RenderVertical m a where
  -- | Draw a figure above the given point.
  drawAbove :: Point -> a -> m Unit

  -- | Draw a figure below the given point.
  drawBelow :: Point -> a -> m Unit

class RenderPoint m a <= RenderHorizontal m a where
  -- | Draw a figure to the left of the given point.
  drawLeft :: Point -> a -> m Unit

  -- | Draw a figure to the right of the given point.
  drawRight :: Point -> a -> m Unit

class RenderPoint m a where
  -- | Draw a figure centered on the given point.
  drawCentered :: Point -> a -> m Unit

newtype HorizontalBelow a = HorizontalBelow a
newtype HorizontalAbove a = HorizontalAbove a
newtype HorizontalCentered a = HorizontalCentered a
newtype VerticalLeft a = VerticalLeft a
newtype VerticalRight a = VerticalRight a
newtype VerticalCentered a = VerticalCentered a

derive instance Newtype (HorizontalBelow a) _
derive instance Newtype (HorizontalAbove a) _
derive instance Newtype (HorizontalCentered a) _
derive instance Newtype (VerticalLeft a) _
derive instance Newtype (VerticalRight a) _
derive instance Newtype (VerticalCentered a) _

derive newtype instance RenderPoint m a => RenderPoint m (HorizontalBelow a)
derive newtype instance RenderPoint m a => RenderPoint m (HorizontalAbove a)
derive newtype instance RenderPoint m a => RenderPoint m (HorizontalCentered a)
derive newtype instance RenderPoint m a => RenderPoint m (VerticalLeft a)
derive newtype instance RenderPoint m a => RenderPoint m (VerticalRight a)
derive newtype instance RenderPoint m a => RenderPoint m (VerticalCentered a)

instance RenderOrtogonal m a => RenderHorizontal m (HorizontalBelow a) where
  drawLeft point (HorizontalBelow a) = drawBottomLeft point a
  drawRight point (HorizontalBelow a) = drawBottomRight point a

instance RenderOrtogonal m a => RenderHorizontal m (HorizontalAbove a) where
  drawLeft point (HorizontalAbove a) = drawTopLeft point a
  drawRight point (HorizontalAbove a) = drawTopRight point a

instance RenderOrtogonal m a => RenderHorizontal m (HorizontalCentered a) where
  drawLeft point (HorizontalCentered a) = drawLeftCentered point a
  drawRight point (HorizontalCentered a) = drawRightCentered point a

instance RenderOrtogonal m a => RenderVertical m (VerticalLeft a) where
  drawAbove point (VerticalLeft a) = drawTopLeft point a
  drawBelow point (VerticalLeft a) = drawBottomLeft point a

instance RenderOrtogonal m a => RenderVertical m (VerticalRight a) where
  drawAbove point (VerticalRight a) = drawTopRight point a
  drawBelow point (VerticalRight a) = drawBottomRight point a

instance RenderOrtogonal m a => RenderVertical m (VerticalCentered a) where
  drawAbove point (VerticalCentered a) = drawTopCentered point a
  drawBelow point (VerticalCentered a) = drawBottomCentered point a
