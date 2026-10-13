module Renderer.Page (draw) where

import Prelude

import Algo as Algo
import Renderer as Renderer

draw
  :: forall m
   . Monad m
  => Renderer.Renderer m
  => Algo.State
  -> m Unit
draw algoState = Algo.renderState algoState
