module Architecture.Rythm
  ( Rythm
  , Pulse
  , rythm
  , fromString
  , pulse
  , motif
  , period
  , beatAt
  , take
  , toPulse
  )
  where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe)
import Data.String.CodeUnits as CodeUnits
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable, traverse)
import Partial.Unsafe (unsafePartial)

-- | An endless repetition of a basic sequence of beats (the motif), e.g. the motif `*_X_**X_`
-- | gives `*_X_**X_*_X_**X_…`. `beat` is the kind of beat; a lack of beat (`_`) is just one
-- | more kind of it (for example `Nothing` when the beats are `Maybe a`).
newtype Rythm beat = Rythm (NonEmptyArray beat)

derive newtype instance Eq beat => Eq (Rythm beat)
derive newtype instance Show beat => Show (Rythm beat)
derive newtype instance Functor Rythm
derive newtype instance Foldable Rythm
derive newtype instance Traversable Rythm

-- | The base of a rythm: equal beats of a single kind, `………`.
type Pulse = Rythm Unit

rythm :: forall beat. NonEmptyArray beat -> Rythm beat
rythm = Rythm

-- | Reads a rythm written as its motif, one character per beat (e.g. `*_X_**X_`).
-- | `Nothing` if the text is empty or `readBeat` does not know one of its characters.
fromString :: forall beat. (Char -> Maybe beat) -> String -> Maybe (Rythm beat)
fromString readBeat = map Rythm <<< (NonEmptyArray.fromArray <=< traverse readBeat <<< CodeUnits.toCharArray)

pulse :: Pulse
pulse = Rythm (NonEmptyArray.singleton unit)

-- | The basic sequence that is repeated.
motif :: forall beat. Rythm beat -> NonEmptyArray beat
motif (Rythm beats) = beats

-- | Number of pulse beats after which the rythm repeats itself.
period :: forall beat. Rythm beat -> Int
period (Rythm beats) = NonEmptyArray.length beats

-- | The beat at the given position of the pulse (0 is the first one; negative positions count backwards).
beatAt :: forall beat. Int -> Rythm beat -> beat
beatAt i (Rythm beats) = unsafePartial $ NonEmptyArray.unsafeIndex beats (i `mod` NonEmptyArray.length beats)

-- | The first `n` beats of the rythm.
take :: forall beat. Int -> Rythm beat -> Array beat
take n r
  | n <= 0 = []
  | otherwise = map (\i -> beatAt i r) (Array.range 0 (n - 1))

-- | The pulse the rythm is laid on.
toPulse :: forall beat. Rythm beat -> Pulse
toPulse _ = pulse
