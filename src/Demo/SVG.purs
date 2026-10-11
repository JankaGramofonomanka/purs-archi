module Demo.SVG (draw) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

type SvgAttribute =
  { name :: String
  , value :: String
  }

type SvgElement =
  { tag :: String
  , attributes :: Array SvgAttribute
  }

draw :: Effect Unit
draw = do
  dimensions <- getSvgDimensions
  let centerX = dimensions.width / 2.0
  let centerY = dimensions.height / 2.0
  mountSvg dimensions.width dimensions.height
    [ rectangle (centerX - 100.0) (centerY - 50.0) 100.0 100.0 "#5268d8"
    , rectangle centerX (centerY - 50.0) 100.0 100.0 "#e53935"
    , horizontalLine dimensions.width (centerY - 350.0) (Just "16 12") "butt" "#c2410c"
    , horizontalLine dimensions.width (centerY - 250.0) (Just "0 12") "round" "#0f766e"
    , horizontalLine dimensions.width (centerY - 150.0) (Just "16 8 0 8") "round" "#7e22ce"
    , horizontalLine dimensions.width (centerY - 50.0) Nothing "butt" "#1c2540"
    , verticalLine centerX centerY 100.0 4.0 1.0
    , verticalLine centerX centerY 200.0 6.0 0.1
    , verticalLine centerX centerY 300.0 4.0 0.3
    , verticalLine centerX centerY 400.0 2.0 1.0
    , cornerPoint (centerX - 200.0) (centerY + 50.0)
    , cornerPoint (centerX - 100.0) (centerY + 50.0)
    , cornerPoint (centerX - 200.0) (centerY + 150.0)
    , cornerPoint (centerX - 100.0) (centerY + 150.0)
    ]

rectangle :: Number -> Number -> Number -> Number -> String -> SvgElement
rectangle x y width height fill = element "rect"
  [ numberAttribute "x" x
  , numberAttribute "y" y
  , numberAttribute "width" width
  , numberAttribute "height" height
  , attribute "fill" fill
  ]

horizontalLine :: Number -> Number -> Maybe String -> String -> String -> SvgElement
horizontalLine width y dashArray lineCap stroke =
  element "line"
    ( [ numberAttribute "x1" 0.0
      , numberAttribute "y1" y
      , numberAttribute "x2" width
      , numberAttribute "y2" y
      , attribute "stroke" stroke
      , numberAttribute "stroke-width" 4.0
      ]
        <> case dashArray of
          Nothing -> []
          Just dash -> [ attribute "stroke-dasharray" dash ]
        <> [ attribute "stroke-linecap" lineCap ]
    )

verticalLine :: Number -> Number -> Number -> Number -> Number -> SvgElement
verticalLine centerX centerY offset width opacity = element "line"
  [ numberAttribute "x1" (centerX + offset)
  , numberAttribute "y1" (centerY - 150.0)
  , numberAttribute "x2" (centerX + offset)
  , numberAttribute "y2" (centerY + 150.0)
  , attribute "stroke" "#1c2540"
  , numberAttribute "stroke-width" width
  , numberAttribute "stroke-opacity" opacity
  ]

cornerPoint :: Number -> Number -> SvgElement
cornerPoint x y = element "circle"
  [ numberAttribute "cx" x
  , numberAttribute "cy" y
  , numberAttribute "r" 4.0
  , attribute "fill" "#1c2540"
  ]

element :: String -> Array SvgAttribute -> SvgElement
element tag attributes = { tag, attributes }

attribute :: String -> String -> SvgAttribute
attribute name value = { name, value }

numberAttribute :: String -> Number -> SvgAttribute
numberAttribute name value = { name, value: show value }

foreign import getSvgDimensions :: Effect { width :: Number, height :: Number }

foreign import mountSvg :: Number -> Number -> Array SvgElement -> Effect Unit
