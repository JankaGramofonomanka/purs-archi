module Demo.SVG (draw) where

import Prelude

import Effect (Effect)

draw :: Effect Unit
draw = mountSvg
  """<svg class="demo-square" viewBox="0 0 240 240" role="img" aria-label="Adjacent blue and red squares with a horizontal line">
       <rect x="20" y="70" width="100" height="100" fill="#5268d8" />
       <rect x="120" y="70" width="100" height="100" fill="#e53935" />
       <line x1="0" y1="70" x2="240" y2="70" stroke="#1c2540" stroke-width="2" />
     </svg>"""

foreign import mountSvg :: String -> Effect Unit
