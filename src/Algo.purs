module Algo
  ( State(..)
  , mkState
  , pickLines
  , renderState
  , step
  )
  where

import Prelude

import Control.Monad.State (StateT, get, lift, modify_)
import Data.Array as Array
import Data.Default (def)
import Data.Foldable (for_)
import Data.Newtype (class Newtype, modify)
import Data.Newtype as Newtype
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Renderer (LineStyle(..), Opaqueness(..))
import Renderer as Renderer
import Test.QuickCheck.Gen as Gen

newtype State = State
  { left :: Number
  , right :: Number
  , lines :: Array Number
  }

derive instance newtypeState :: Newtype State _

data Action = AddVertical

mkState :: Number -> Number -> State
mkState left right = State { left, right, lines: [] }

step :: StateT State Gen.Gen Unit
step = do
  action <- chooseAction
  handleAction action

chooseAction :: StateT State Gen.Gen Action
chooseAction = lift $ pure AddVertical

handleAction :: Action -> StateT State Gen.Gen Unit
handleAction AddVertical = addVertical

addVertical :: StateT State Gen.Gen Unit
addVertical = do
  State { lines } <- get
  Tuple left right <- pickLines
  let middle = (right + left) / 2.0
  let newLines = Array.nub (lines <> [middle])
  modify_ (Newtype.modify (_ { lines = newLines }))

pickLines :: StateT State Gen.Gen (Tuple Number Number)
pickLines = do
  Tuple one two <- pickTwo
  pure $ Tuple (min one two) (max one two)

pickTwo :: StateT State Gen.Gen (Tuple Number Number)
pickTwo = do
  State { left, right, lines } <- get
  shuffled <- lift $ Gen.shuffle (Array.nub ([left, right] <> lines))
  pure $ unsafePartial $ Tuple
    (Array.unsafeIndex shuffled 0)
    (Array.unsafeIndex shuffled 1)

renderState :: forall m. Monad m => Renderer.Renderer m => State -> m Unit
renderState (State { left, right, lines }) = do
  drawBound left
  drawBound right
  for_ lines \line -> do
    drawLine line

  where
    drawBound line =
      Renderer.drawLineVertical line (modify (_ { opaqueness = Opaqueness 0.3, lineStyle = DashDot }) def)
    drawLine line = Renderer.drawLineVertical line def
