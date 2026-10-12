module Algo
  ( State(..)
  , mkState
  , pickLines
  , step
  )
  where

import Prelude

import Control.Monad.State (StateT, gets, modify_)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Random (randomInt)
import Data.Newtype as Newtype
import Data.Newtype (class Newtype, unwrap)

newtype State = State
  { left :: Number
  , right :: Number
  , lines :: Array Number
  }

derive instance newtypeState :: Newtype State _

mkState :: Number -> Number -> State
mkState left right = State { left, right, lines: [left, right] }

step :: forall m. MonadEffect m => StateT State m Unit
step = do
  Tuple left right <- pickLines
  let middle = right + left / 2.0
  lines <- gets (_.lines <<< unwrap)
  modify_ (Newtype.modify (_ { lines = lines <> [middle] }))

pickLines :: forall m. MonadEffect m => StateT State m (Tuple Number Number)
pickLines = do
  lines <- gets (_.lines <<< unwrap)
  Tuple one two <- pickTwo lines
  pure $ Tuple (min one two) (max one two)

pickTwo
  :: forall m
   . MonadEffect m
  => Array Number
  -> m (Tuple Number Number)
pickTwo values
  | Array.length values < 2 = liftEffect $ throw "pickTwo requires at least two lines"
  | otherwise = do
      let lastIndex = Array.length values - 1
      firstIndex <- liftEffect $ randomInt 0 lastIndex
      secondIndex <- liftEffect $ randomInt 0 (lastIndex - 1)
      case Array.index values firstIndex,
           Array.index values (if secondIndex == firstIndex then secondIndex + 1 else secondIndex) of
        Just first, Just second -> pure $ Tuple first second
        _, _ -> liftEffect $ throw "pickTwo could not retrieve randomly selected lines"
