module SVG.Page (draw) where

import Prelude

import Effect (Effect)
import Renderer.Page as Page
import SVG.Impl as Renderer
import Algo as Algo

draw :: Algo.State -> Effect Unit
draw algoState = do
  void Renderer.prepareSvg
  Renderer.runSVGRenderer (Page.draw algoState)
