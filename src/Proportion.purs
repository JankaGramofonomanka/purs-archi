module Proportion where

import Prelude

class Measurable object measure unit where
  measure :: object -> measure -> unit

newtype Proportion measure1 measure2 = Proportion { measure1 :: measure1, measure2 :: measure2, ratio :: Ratio }
type OwnProportion measure = Proportion measure measure

-- | An exact, positive ratio.
type Ratio = { numerator :: Int, denominator :: Int }
