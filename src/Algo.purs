module Algo
  ( State(..)
  , mkState
  , renderState
  , step
  )
  where

import Prelude

import Control.Monad.State (StateT, get, lift, modify_)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Default (def)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, modify, unwrap)
import Data.Newtype as Newtype
import Data.Number as Number
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

data Action = AddVertical | AddHorizontal

applicable :: State -> Action -> Boolean
applicable (State state) AddVertical =
  Number.abs (state.currentRight - state.currentLeft) > 12.0
applicable (State state) AddHorizontal =
  Number.abs (state.currentBottom - state.currentTop) > 12.0

allActions :: Array Action
allActions = [ AddVertical, AddHorizontal ]

step :: StateT State Gen.Gen Unit
step = do
  m'action <- chooseAction
  case m'action of
    Nothing -> pure unit
    Just action -> handleAction action

chooseAction :: StateT State Gen.Gen (Maybe Action)
chooseAction = do
  state <- get
  case NonEmptyArray.fromArray (Array.filter (applicable state) allActions) of
    Nothing -> pure Nothing
    Just applicableActions -> Just <$> (lift $ Gen.elements applicableActions)

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
