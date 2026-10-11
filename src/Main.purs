module Main where

import Prelude

import Effect (Effect)
import Effect.Ref as Ref

main :: Effect Unit
main = do
  count <- Ref.new 0
  setCount 0
  onIncrement do
    next <- Ref.modify (_ + 1) count
    setCount next

foreign import setCount :: Int -> Effect Unit

foreign import onIncrement :: Effect Unit -> Effect Unit
