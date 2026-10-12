module Ortogonal where

import Prelude

import Renderer (Point(..))

class RenderOrtogonal m a where
  -- | Draw a figure centered on the given point.
  drawCentered :: Point -> a -> m Unit

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
