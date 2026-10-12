module SVG.Clear (clear) where

import Prelude

import Effect (Effect)
import SVG.Impl as Renderer

clear :: Effect Unit
clear = void Renderer.prepareSvg
