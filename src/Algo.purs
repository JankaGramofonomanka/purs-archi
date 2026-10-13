module Algo
  ( State(..)
  , mkState
  , renderState
  , step
  )
  where

import Prelude

import Control.Monad.State (StateT, get, gets, lift, modify_)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Default (def)
import Data.Foldable (for_)
import Data.Newtype (class Newtype, modify, unwrap)
import Data.Newtype as Newtype
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Renderer (LineStyle(..), Opaqueness(..))
import Renderer as Renderer
import Test.QuickCheck.Gen as Gen

newtype State = State
  { left :: Number
  , right :: Number
  , top :: Number
  , bottom :: Number
  , vertical :: Array Number
  , horizontal :: Array Number
  , currentLeft :: Number
  , currentRight :: Number
  , currentTop :: Number
  , currentBottom :: Number
  }

derive instance newtypeState :: Newtype State _

data Action = AddVertical | AddHorizontal

mkState :: Number -> Number -> Number -> Number -> State
mkState left right top bottom
  = State
  { left
  , right
  , top
  , bottom
  , vertical: []
  , horizontal: []
  , currentLeft: left
  , currentRight: right
  , currentTop: top
  , currentBottom: bottom
  }

step :: StateT State Gen.Gen Unit
step = do
  action <- chooseAction
  handleAction action

chooseAction :: StateT State Gen.Gen Action
chooseAction = lift $ Gen.elements
  (NonEmptyArray.cons' AddVertical [AddHorizontal])

handleAction :: Action -> StateT State Gen.Gen Unit
handleAction AddVertical = addVertical
handleAction AddHorizontal = addHorizontal

addVertical :: StateT State Gen.Gen Unit
addVertical = do
  State { vertical, currentLeft, currentRight } <- get
  let middle = (currentLeft + currentRight) / 2.0
  side <- lift $ Gen.elements (NonEmptyArray.cons' currentLeft [ currentRight ])

  let left = min side middle
  let right = max side middle
  let newVertical = [middle] <> vertical
  modify_ (Newtype.modify (_ { vertical = newVertical, currentLeft = left, currentRight = right }))

addHorizontal :: StateT State Gen.Gen Unit
addHorizontal = do
  State { horizontal, currentTop, currentBottom } <- get
  let middle = (currentTop + currentBottom) / 2.0
  side <- lift $ Gen.elements (NonEmptyArray.cons' currentTop [ currentBottom ])

  let top = max side middle
  let bottom = min side middle
  let newHorizontal = [middle] <> horizontal
  modify_ (Newtype.modify (_ { horizontal = newHorizontal, currentTop = top, currentBottom = bottom }))

pickTwo
  :: (State -> Number)
  -> (State -> Number)
  -> (State -> Array Number)
  -> StateT State Gen.Gen (Tuple Number Number)
pickTwo getLowerBound getUpperBound getLines = do
  left <- gets getLowerBound
  right <- gets getUpperBound
  lines <- gets getLines
  shuffled <- lift $ Gen.shuffle (Array.nub ([left, right] <> lines))
  pure $ unsafePartial $ Tuple
    (Array.unsafeIndex shuffled 0)
    (Array.unsafeIndex shuffled 1)

renderState :: forall m. Monad m => Renderer.Renderer m => State -> m Unit
renderState (State { left, right, top, bottom, vertical, horizontal }) = do
  drawBoundVertical left
  drawBoundVertical right
  for_ vertical drawLineVertical
  drawBoundVertical left
  drawBoundVertical right
  drawBoundHorizontal top
  drawBoundHorizontal bottom
  for_ horizontal drawLineHorizontal

  where
    boundContour = modify (_ { opaqueness = Opaqueness 0.3, lineStyle = DashDot }) def
    drawBoundVertical   line = Renderer.drawLineVertical   line boundContour
    drawBoundHorizontal line = Renderer.drawLineHorizontal line boundContour
    drawLineVertical    line = Renderer.drawLineVertical   line def
    drawLineHorizontal  line = Renderer.drawLineHorizontal line def
