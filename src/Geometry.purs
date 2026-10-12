module Geometry
  ( Rectangle(..)
  ) where

import Prelude

import Data.Default (def)
import Data.Maybe (Maybe(..))
import Ortogonal (class RenderOrtogonal)
import Renderer (Point(..), point)
import Renderer as Renderer

data Rectangle = Rectangle
  { width :: Number
  , height :: Number
  }

instance Renderer.Renderer m => RenderOrtogonal m Rectangle where
  drawCentered (Point p) (Rectangle { width, height }) =
    drawRectangleAt (p.x - width / 2.0) (p.y - height / 2.0) width height

  drawLeftCentered (Point p) (Rectangle { width, height }) =
    drawRectangleAt (p.x - width) (p.y - height / 2.0) width height

  drawRightCentered (Point p) (Rectangle { width, height }) =
    drawRectangleAt p.x (p.y - height / 2.0) width height

  drawTopCentered (Point p) (Rectangle { width, height }) =
    drawRectangleAt (p.x - width / 2.0) (p.y - height) width height

  drawBottomCentered (Point p) (Rectangle { width, height }) =
    drawRectangleAt (p.x - width / 2.0) p.y width height

  drawTopLeft (Point p) (Rectangle { width, height }) =
    drawRectangleAt (p.x - width) (p.y - height) width height

  drawTopRight (Point p) (Rectangle { width, height }) =
    drawRectangleAt p.x (p.y - height) width height

  drawBottomLeft (Point p) (Rectangle { width, height }) =
    drawRectangleAt (p.x - width) p.y width height

  drawBottomRight (Point p) (Rectangle { width, height }) =
    drawRectangleAt p.x p.y width height

drawRectangleAt :: forall m. Renderer.Renderer m => Number -> Number -> Number -> Number -> m Unit
drawRectangleAt x y width height =
  Renderer.drawRectangle (point x y) width height (Just def) Nothing
