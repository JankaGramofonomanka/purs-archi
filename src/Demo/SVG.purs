module Demo.SVG (draw) where

import Prelude

import Effect (Effect)

draw :: Effect Unit
draw = mountSvg
  """<svg class="demo-square" viewBox="0 0 240 240" role="img" aria-label="Adjacent blue and red squares drawn with SVG">
       <rect x="20" y="70" width="100" height="100" fill="#5268d8" />
       <rect x="120" y="70" width="100" height="100" fill="#e53935" />
     </svg>"""

foreign import mountSvg :: String -> Effect Unit
