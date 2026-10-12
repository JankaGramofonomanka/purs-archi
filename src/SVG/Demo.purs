module SVG.Demo (draw) where

import Prelude

import Effect (Effect)
import Renderer.Demo as Demo
import SVG.Impl as Renderer

draw :: Effect Unit
draw = do
  dimensions <- Renderer.prepareSvg
  Renderer.runSVGRenderer (Demo.draw dimensions.width dimensions.height)
