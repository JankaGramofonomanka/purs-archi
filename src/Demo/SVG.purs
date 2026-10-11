module Demo.SVG (draw) where

import Prelude

import Effect (Effect)

draw :: Effect Unit
draw = mountSvg
  """<svg class="demo-square" role="img" aria-label="Drawing demo">
       <rect x="50%" y="50%" width="100" height="100" transform="translate(-100 -50)" fill="#5268d8" />
       <rect x="50%" y="50%" width="100" height="100" transform="translate(0 -50)" fill="#e53935" />
       <line x1="0" y1="50%" x2="100%" y2="50%" transform="translate(0 -50)" stroke="#1c2540" stroke-width="4" />
       <line x1="calc(50% + 100px)" y1="calc(50% - 150px)" x2="calc(50% + 100px)" y2="calc(50% + 150px)" stroke="#1c2540" stroke-width="4" />
       <line x1="calc(50% + 200px)" y1="calc(50% - 150px)" x2="calc(50% + 200px)" y2="calc(50% + 150px)" stroke="#1c2540" stroke-width="6" />
       <line x1="calc(50% + 300px)" y1="calc(50% - 150px)" x2="calc(50% + 300px)" y2="calc(50% + 150px)" stroke="#1c2540" stroke-width="4" />
       <line x1="calc(50% + 400px)" y1="calc(50% - 150px)" x2="calc(50% + 400px)" y2="calc(50% + 150px)" stroke="#1c2540" stroke-width="2" />
     </svg>"""

foreign import mountSvg :: String -> Effect Unit
