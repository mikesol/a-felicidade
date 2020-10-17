module Klank.Bass where

import Prelude
import Control.Promise (toAffE)
import Data.Array (foldl, head, last, length, mapWithIndex, range, span, take, zipWith)
import Data.Int (toNumber)
import Data.List (List(..))
import Data.List as L
import Data.Maybe (fromMaybe)
import Data.NonEmpty ((:|))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1, D2)
import Effect (Effect)
import Effect.Aff (Error, ParAff, parallel, sequential)
import Effect.Random (random)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioParameter(..), AudioUnit, BrowserAudioBuffer, decodeAudioDataFromUri, gain', gainT', gainT_', playBufT_, runInBrowser, sinOsc, speaker, speaker')
import Foreign.Object as O
import Math (pow)
import Type.Klank.Dev (Klank, affable, klank)

kr = 20.0 / 1000.0 :: Number

pwf :: Number -> Array (Tuple Number Number)
pwf s =
  [ Tuple 0.0 0.0 ] <> (if s <= 0.0 then [] else [ Tuple s 0.0 ])
    <> [ Tuple (s + 0.024) 0.94
      , Tuple (s + 0.15) 0.5
      , Tuple (s + 0.46) 0.1
      , Tuple (s + 0.76) 0.0
      ]

split :: ∀ t12 t13. Ord t12 ⇒ t12 → Array (Tuple t12 t13) → { init ∷ Array (Tuple t12 t13), rest ∷ Array (Tuple t12 t13) }
split s p = span ((s >= _) <<< fst) p

gn :: Number → Array (Tuple Number Number) → AudioParameter Number
gn s p =
  let
    ht = split s p

    left = fromMaybe (Tuple 0.0 0.0) $ last ht.init

    right = fromMaybe (Tuple 101.0 0.0) $ head ht.rest
  in
    if (fst right - s) < kr then
      AudioParameter
        { param: (snd right)
        , timeOffset: (fst right - s)
        }
    else
      let
        m = (snd right - snd left) / (fst right - fst left)

        b = (snd right - (m * fst right))
      in
        AudioParameter { param: (m * s + b), timeOffset: 0.0 }

midi2cps :: Number -> Number
midi2cps n = (440.0 * (2.0 `pow` ((n - 69.0) / 12.0)))

scene :: Number -> Behavior (AudioUnit D1)
scene t = pure (speaker' (gainT' (gn t (pwf 0.2)) $ sinOsc (midi2cps 25.0)))

main :: Klank
main = klank { run = runInBrowser scene }
