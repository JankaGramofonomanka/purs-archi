module Demo.SVG (draw) where

import Prelude

import Effect (Effect)

draw :: Effect Unit
draw = mountSvg
  """<svg class="demo-square" viewBox="0 0 240 240" role="img" aria-label="Square drawn with SVG">
       <rect x="60" y="60" width="120" height="120" fill="#5268d8" />
     </svg>"""

foreign import mountSvg :: String -> Effect Unit
