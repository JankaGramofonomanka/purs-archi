module SVG.Page (draw) where

import Prelude

import Effect (Effect)
import Renderer.Page as Page
import SVG.Impl as Renderer

draw :: Number -> Effect Unit
draw radius = do
  dimensions <- Renderer.prepareSvg
  Renderer.runSVGRenderer (Page.draw dimensions.width dimensions.height radius)
