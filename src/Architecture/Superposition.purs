module Architecture.Superposition
  ( Ratio
  , SuperpositionRule(..)
  , continuousTaper
  , equalStoreys
  , fourthLess
  , ratioCodec
  , ratioValue
  , superpositionRuleCodec
  )
  where

import Prelude

import Architecture.Measure (Measure(..), measureCodec)
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJR
import Data.Codec.JSON.Sum as CJS
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | An exact, positive ratio.
type Ratio = { numerator :: Int, denominator :: Int }

ratioValue :: Ratio -> Number
ratioValue { numerator, denominator } = Int.toNumber numerator / Int.toNumber denominator

-- | How large an order stacked on another is.
-- | `Match { upper, lower, ratio }`: the `upper` measure of the upper storey is `ratio` times the
-- | `lower` measure of the storey below. Every rule must determine the lower storey from the
-- | upper one as well, since a stack may be sized from any of its storeys.
data SuperpositionRule = Match { upper :: Measure, lower :: Measure, ratio :: Ratio }

derive instance Eq SuperpositionRule

-- | Scamozzi's rule: the lower diameter of the upper column is the upper diameter of the
-- | lower one, "as if from one long single tree".
continuousTaper :: SuperpositionRule
continuousTaper = Match { upper: LowerDiameter, lower: UpperDiameter, ratio: { numerator: 1, denominator: 1 } }

-- | Vitruvius' rule (Book V, chapters 1 and 7): the upper columns are a fourth less than the lower.
fourthLess :: SuperpositionRule
fourthLess = Match { upper: ColumnHeight, lower: ColumnHeight, ratio: { numerator: 3, denominator: 4 } }

-- | Storeys of equal height, pedestals included.
equalStoreys :: SuperpositionRule
equalStoreys = Match { upper: StoreyHeight, lower: StoreyHeight, ratio: { numerator: 1, denominator: 1 } }

-- Codecs

-- | A ratio as a pair `[numerator, denominator]` of positive integers, e.g. `[3, 4]`.
ratioCodec :: CJ.Codec Ratio
ratioCodec = CJ.prismaticCodec "Ratio" fromPair toPair (CJ.array CJ.int)
  where
  fromPair = case _ of
    [ numerator, denominator ] | numerator > 0 && denominator > 0 -> Just { numerator, denominator }
    _ -> Nothing
  toPair { numerator, denominator } = [ numerator, denominator ]

data RuleTag = MatchTag

-- | A rule as `{ "tag": "match", "value": { "upper": …, "lower": …, "ratio": [n, d] } }`.
superpositionRuleCodec :: CJ.Codec SuperpositionRule
superpositionRuleCodec = CJS.taggedSum "SuperpositionRule" printTag parseTag decodeCase encodeCase
  where
  printTag MatchTag = "match"
  parseTag = case _ of
    "match" -> Just MatchTag
    _ -> Nothing
  decodeCase MatchTag = Right (map Match <<< CJ.decode matchCodec)
  encodeCase (Match m) = Tuple MatchTag (Just (CJ.encode matchCodec m))
  matchCodec = CJR.object { upper: measureCodec, lower: measureCodec, ratio: ratioCodec }
