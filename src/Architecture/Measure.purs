module Architecture.Measure
  ( CollonadeMeasure(..)
  , Measure(..)
  , SizeDeterminant
  , collonadeMeasureCodec
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
-- | `Minute` is one minute; `OrderHeight` is the column with its entablature.
data Measure = Minute | LowerDiameter | UpperDiameter | ColumnHeight | EntablatureHeight | OrderHeight

derive instance Eq Measure

-- | A dimension of a row of columns: `StoreyHeight` is its order with the pedestal the columns stand
-- | on, if they stand on one; `FromOrder` is a measure of its order.
data CollonadeMeasure = StoreyHeight | FromOrder Measure

derive instance Eq CollonadeMeasure

-- | The size of an order, given as the length of one of its measures.
type SizeDeterminant = { measure :: Measure, value :: Number }

-- | A measure as a string, e.g. `"lowerDiameter"`.
measureCodec :: CJ.Codec Measure
measureCodec = CJS.enumSum printMeasure parseMeasure

printMeasure :: Measure -> String
printMeasure = case _ of
  Minute -> "minute"
  LowerDiameter -> "lowerDiameter"
  UpperDiameter -> "upperDiameter"
  ColumnHeight -> "columnHeight"
  EntablatureHeight -> "entablatureHeight"
  OrderHeight -> "orderHeight"

parseMeasure :: String -> Maybe Measure
parseMeasure = case _ of
  "minute" -> Just Minute
  "lowerDiameter" -> Just LowerDiameter
  "upperDiameter" -> Just UpperDiameter
  "columnHeight" -> Just ColumnHeight
  "entablatureHeight" -> Just EntablatureHeight
  "orderHeight" -> Just OrderHeight
  _ -> Nothing

-- | A measure of a row of columns as a string: `"storeyHeight"`, or a measure of its order.
collonadeMeasureCodec :: CJ.Codec CollonadeMeasure
collonadeMeasureCodec = CJ.prismaticCodec "CollonadeMeasure" parse print CJ.string
  where
  print = case _ of
    StoreyHeight -> "storeyHeight"
    FromOrder measure -> printMeasure measure
  parse = case _ of
    "storeyHeight" -> Just StoreyHeight
    string -> FromOrder <$> parseMeasure string

-- | A size determinant as `{ "measure": "lowerDiameter", "value": 10 }`.
sizeDeterminantCodec :: CJ.Codec SizeDeterminant
sizeDeterminantCodec = CJR.object { measure: measureCodec, value: CJ.number }
