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
  , endOnFirstBeat
  , dropTrailing
  , toPulse
  )
  where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe)
import Data.String (Pattern(..))
import Data.String as String
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

-- | Reads a rythm written as its motif, the beats separated by `separator` (e.g. `*:_:X:_` with `":"`).
-- | `Nothing` if `readBeat` does not know one of the beats.
fromString :: forall beat. String -> (String -> Maybe beat) -> String -> Maybe (Rythm beat)
fromString separator readBeat = map Rythm <<< (NonEmptyArray.fromArray <=< traverse readBeat <<< String.split (Pattern separator))

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

-- | `n` rounded down to whole periods of the rythm and the first beat of the next one (`k * period + 1`),
-- | so that the beats end as they start. `n` below 1 is left as it is.
endOnFirstBeat :: forall beat. Int -> Rythm beat -> Int
endOnFirstBeat n r
  | n < 1 = n
  | otherwise = n - (n - 1) `mod` period r

-- | How many of the first `n` beats of the rythm are left once the trailing beats that `isBlank`
-- | says are blank are dropped; the first beat is always kept.
dropTrailing :: forall beat. (beat -> Boolean) -> Int -> Rythm beat -> Int
dropTrailing isBlank n r
  | n > 1 && isBlank (beatAt (n - 1) r) = dropTrailing isBlank (n - 1) r
  | otherwise = n

-- | The pulse the rythm is laid on.
toPulse :: forall beat. Rythm beat -> Pulse
toPulse _ = pulse
