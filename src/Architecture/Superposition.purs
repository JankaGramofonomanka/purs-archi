module Architecture.Superposition
  ( Step
  , SuperpositionRule(..)
  , albertiDiminution
  , continuousTaper
  , equalStoreys
  , fourthLess
  , storeyFourthLess
  , stepAt
  , ratioCodec
  , ratioValue
  , stepCodec
  , superpositionRuleCodec
  )
  where

import Prelude

import Architecture.Measure (CollonadeMeasure(..), Measure(..), collonadeMeasureCodec)
import Data.Codec.JSON as CJ
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.JSON.Common as CJC
import Data.Codec.JSON.Record as CJR
import Data.Codec.JSON.Sum as CJS
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Proportion (Ratio)

ratioValue :: Ratio -> Number
ratioValue { numerator, denominator } = Int.toNumber numerator / Int.toNumber denominator

-- | How large one storey is relative to the storey below: its `upper` measure is `ratio` times
-- | the `lower` measure of the storey below. A step determines the lower storey from the upper
-- | one as well, since a stack may be sized from any of its storeys.
type Step = { upper :: CollonadeMeasure, lower :: CollonadeMeasure, ratio :: Ratio }

-- | How large an order stacked on another is.
-- | `Match step`: the same step between every two storeys.
-- | `Successive steps`: the first step between the first and the second storey, the second between
-- | the second and the third, and so on; the last step repeats above.
data SuperpositionRule = Match Step | Successive (NonEmptyArray Step)

derive instance Eq SuperpositionRule

-- | The step between storey `joint` and storey `joint + 1`, counting from 0 at the bottom.
stepAt :: Int -> SuperpositionRule -> Step
stepAt joint = case _ of
  Match step -> step
  Successive steps -> fromMaybe (NonEmptyArray.last steps) (NonEmptyArray.index steps joint)

-- | Scamozzi's rule: the lower diameter of the upper column is the upper diameter of the
-- | lower one, "as if from one long single tree".
continuousTaper :: SuperpositionRule
continuousTaper = Match { upper: FromOrder LowerDiameter, lower: FromOrder UpperDiameter, ratio: { numerator: 1, denominator: 1 } }

-- | Vitruvius' rule (Book V, chapters 1 and 7): the upper columns are a fourth less than the lower.
fourthLess :: SuperpositionRule
fourthLess = Match { upper: FromOrder ColumnHeight, lower: FromOrder ColumnHeight, ratio: { numerator: 3, denominator: 4 } }

-- | Serlio's reading of Vitruvius: the whole upper storey, from its floor to the top of its cornice,
-- | is a fourth less than the storey below, every member diminished in proportion.
storeyFourthLess :: SuperpositionRule
storeyFourthLess = Match { upper: StoreyHeight, lower: StoreyHeight, ratio: { numerator: 3, denominator: 4 } }

-- | Alberti's rule (Book IX): the second order a fourth shorter than the first, and a third
-- | order a fifth shorter than the second.
albertiDiminution :: SuperpositionRule
albertiDiminution = Successive $ NonEmptyArray.cons'
  { upper: FromOrder OrderHeight, lower: FromOrder OrderHeight, ratio: { numerator: 3, denominator: 4 } }
  [ { upper: FromOrder OrderHeight, lower: FromOrder OrderHeight, ratio: { numerator: 4, denominator: 5 } } ]

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

-- | A step as `{ "upper": …, "lower": …, "ratio": [n, d] }`.
stepCodec :: CJ.Codec Step
stepCodec = CJR.object { upper: collonadeMeasureCodec, lower: collonadeMeasureCodec, ratio: ratioCodec }

data RuleTag = MatchTag | SuccessiveTag

-- | A rule as `{ "tag": "match", "value": step }` or `{ "tag": "successive", "value": [step, …] }`.
superpositionRuleCodec :: CJ.Codec SuperpositionRule
superpositionRuleCodec = CJS.taggedSum "SuperpositionRule" printTag parseTag decodeCase encodeCase
  where
  printTag = case _ of
    MatchTag -> "match"
    SuccessiveTag -> "successive"
  parseTag = case _ of
    "match" -> Just MatchTag
    "successive" -> Just SuccessiveTag
    _ -> Nothing
  decodeCase = case _ of
    MatchTag -> Right (map Match <<< CJ.decode stepCodec)
    SuccessiveTag -> Right (map Successive <<< CJ.decode stepsCodec)
  encodeCase = case _ of
    Match step -> Tuple MatchTag (Just (CJ.encode stepCodec step))
    Successive steps -> Tuple SuccessiveTag (Just (CJ.encode stepsCodec steps))
  stepsCodec = CJC.nonEmptyArray stepCodec
