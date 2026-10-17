module SVG.Collonade (draw) where

import Prelude

import Effect (Effect)
import Renderer.Collonade as Collonade
import SVG.Impl as Renderer

draw :: Collonade.Config -> Effect Unit
draw config = do
  dimensions <- Renderer.prepareSvg
  Renderer.runSVGRenderer (Collonade.draw config dimensions.width dimensions.height)
