module Architecture.Measure
  ( Measure(..)
  , SizeDeterminant
  , measureCodec
  , sizeDeterminantCodec
  )
  where

import Prelude

import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJR
import Data.Codec.JSON.Sum as CJS
import Data.Maybe (Maybe(..))

-- | A dimension of an order, in minutes of that order.
-- | `Minute` is one minute; `OrderHeight` is the column with its entablature; `StoreyHeight` adds
-- | the pedestal the columns stand on, if they stand on one.
data Measure = Minute | LowerDiameter | UpperDiameter | ColumnHeight | EntablatureHeight | OrderHeight | StoreyHeight

derive instance Eq Measure

-- | The size of an order, given as the length of one of its measures.
type SizeDeterminant = { measure :: Measure, value :: Number }

-- | A measure as a string, e.g. `"lowerDiameter"`.
measureCodec :: CJ.Codec Measure
measureCodec = CJS.enumSum print parse
  where
  print = case _ of
    Minute -> "minute"
    LowerDiameter -> "lowerDiameter"
    UpperDiameter -> "upperDiameter"
    ColumnHeight -> "columnHeight"
    EntablatureHeight -> "entablatureHeight"
    OrderHeight -> "orderHeight"
    StoreyHeight -> "storeyHeight"
  parse = case _ of
    "minute" -> Just Minute
    "lowerDiameter" -> Just LowerDiameter
    "upperDiameter" -> Just UpperDiameter
    "columnHeight" -> Just ColumnHeight
    "entablatureHeight" -> Just EntablatureHeight
    "orderHeight" -> Just OrderHeight
    "storeyHeight" -> Just StoreyHeight
    _ -> Nothing

-- | A size determinant as `{ "measure": "lowerDiameter", "value": 10 }`.
sizeDeterminantCodec :: CJ.Codec SizeDeterminant
sizeDeterminantCodec = CJR.object { measure: measureCodec, value: CJ.number }
