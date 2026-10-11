module Demo.SVG (draw) where

import Prelude

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
draw = mountSvg
  [ rectangle "50%" "50%" 100.0 100.0 "translate(-100 -50)" "#5268d8"
  , rectangle "50%" "50%" 100.0 100.0 "translate(0 -50)" "#e53935"
  , horizontalLine
  , verticalLine 100.0 4.0
  , verticalLine 200.0 6.0
  , verticalLine 300.0 4.0
  , verticalLine 400.0 2.0
  ]

rectangle :: String -> String -> Number -> Number -> String -> String -> SvgElement
rectangle x y width height transform fill = element "rect"
  [ attribute "x" x
  , attribute "y" y
  , attribute "width" (show width)
  , attribute "height" (show height)
  , attribute "transform" transform
  , attribute "fill" fill
  ]

horizontalLine :: SvgElement
horizontalLine = element "line"
  [ attribute "x1" "0"
  , attribute "y1" "50%"
  , attribute "x2" "100%"
  , attribute "y2" "50%"
  , attribute "transform" "translate(0 -50)"
  , attribute "stroke" "#1c2540"
  , attribute "stroke-width" "4"
  ]

verticalLine :: Number -> Number -> SvgElement
verticalLine offset width = element "line"
  [ attribute "x1" (centerOffset offset)
  , attribute "y1" (centerOffset (-150.0))
  , attribute "x2" (centerOffset offset)
  , attribute "y2" (centerOffset 150.0)
  , attribute "stroke" "#1c2540"
  , attribute "stroke-width" (show width)
  ]

centerOffset :: Number -> String
centerOffset offset =
  if offset < 0.0 then
    "calc(50% - " <> show (0.0 - offset) <> "px)"
  else
    "calc(50% + " <> show offset <> "px)"

element :: String -> Array SvgAttribute -> SvgElement
element tag attributes = { tag, attributes }

attribute :: String -> String -> SvgAttribute
attribute name value = { name, value }

foreign import mountSvg :: Array SvgElement -> Effect Unit
