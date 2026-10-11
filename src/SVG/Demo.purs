module SVG.Demo (draw) where

import Prelude

import Effect (Effect)
import Geometry.Demo as Demo
import SVG.Impl as Renderer

draw :: Effect Unit
draw = do
  dimensions <- Renderer.prepareSvg
  Renderer.runRenderer (Demo.draw dimensions.width dimensions.height)
