module Renderer.Page (draw) where

import Prelude

import Data.Default (class Default, def)
import Data.Maybe (Maybe(..))
import Renderer as Renderer

draw
  :: forall m
   . Renderer.Renderer m
  => Default Renderer.Contour
  => Number
  -> Number
  -> Number
  -> m Unit
draw width height radius =
  Renderer.drawCircle (Renderer.point (width / 2.0) (height / 2.0)) radius
    (Just def) Nothing
