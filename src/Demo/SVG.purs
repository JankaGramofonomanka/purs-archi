module Demo.SVG (draw) where

import Prelude

import Effect (Effect)

draw :: Effect Unit
draw = do
  dimensions <- getSvgDimensions
  let centerX = dimensions.width / 2.0
  let centerY = dimensions.height / 2.0
  let svg =
        "<svg class=\"demo-square\" viewBox=\"0 0 "
          <> show dimensions.width
          <> " "
          <> show dimensions.height
          <> "\" role=\"img\" aria-label=\"Drawing demo\">"
          <> "<rect x=\""
          <> show (centerX - 100.0)
          <> "\" y=\""
          <> show (centerY - 50.0)
          <> "\" width=\"100\" height=\"100\" fill=\"#5268d8\" />"
          <> "<rect x=\""
          <> show centerX
          <> "\" y=\""
          <> show (centerY - 50.0)
          <> "\" width=\"100\" height=\"100\" fill=\"#e53935\" />"
          <> "<line x1=\"0\" y1=\""
          <> show (centerY - 50.0)
          <> "\" x2=\""
          <> show dimensions.width
          <> "\" y2=\""
          <> show (centerY - 50.0)
          <> "\" stroke=\"#1c2540\" stroke-width=\"4\" />"
          <> verticalLine centerX 100.0 4.0 centerY
          <> verticalLine centerX 200.0 6.0 centerY
          <> verticalLine centerX 300.0 4.0 centerY
          <> verticalLine centerX 400.0 2.0 centerY
          <> "</svg>"
  mountSvg svg
  where
  verticalLine centerX offset width centerY =
    "<line x1=\""
      <> show (centerX + offset)
      <> "\" y1=\""
      <> show (centerY - 150.0)
      <> "\" x2=\""
      <> show (centerX + offset)
      <> "\" y2=\""
      <> show (centerY + 150.0)
      <> "\" stroke=\"#1c2540\" stroke-width=\""
      <> show width
      <> "\" />"

foreign import mountSvg :: String -> Effect Unit

foreign import getSvgDimensions :: Effect { width :: Number, height :: Number }
