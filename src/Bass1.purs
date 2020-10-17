module Klank.Bass where

import Prelude
import Control.Promise (toAffE)
import Data.Array (foldl, head, last, length, mapWithIndex, range, span, take, zipWith)
import Data.Int (toNumber)
import Data.List (List(..), (:))
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
import FRP.Behavior.Audio (AudioContext, AudioParameter(..), AudioUnit, BrowserAudioBuffer, decodeAudioDataFromUri, gain, gain', gainT, gainT', gainT_', playBufT_, runInBrowser, sinOsc, sinOsc_, speaker, speaker')
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

pwfSub :: Number -> Array (Tuple Number Number)
pwfSub s =
  [ Tuple 0.0 0.0 ] <> (if s <= 0.0 then [] else [ Tuple s 0.0 ])
    <> [ Tuple (s + 0.024) 0.3
      , Tuple (s + 0.08) 0.5
      , Tuple (s + 0.23) 0.1
      , Tuple (s + 0.51) 0.0
      ]

pwfH1 :: Number -> Array (Tuple Number Number)
pwfH1 s =
  [ Tuple 0.0 0.0 ] <> (if s <= 0.0 then [] else [ Tuple s 0.0 ])
    <> [ Tuple (s + 0.024) 0.5
      , Tuple (s + 0.08) 0.15
      , Tuple (s + 0.23) 0.05
      , Tuple (s + 0.51) 0.0
      ]

pwfH2 :: Number -> Array (Tuple Number Number)
pwfH2 s =
  [ Tuple 0.0 0.0 ] <> (if s <= 0.0 then [] else [ Tuple s 0.0 ])
    <> [ Tuple (s + 0.024) 0.05
      , Tuple (s + 0.08) 0.003
      , Tuple (s + 0.11) 0.001
      , Tuple (s + 0.13) 0.0
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

thunk :: Number → Number → String → AudioUnit D1
thunk t gg tag =
  ( gain gg
      ( (gainT_' ("g0" <> tag) (gn t (pwf 1.0)) $ sinOsc_ ("s0" <> tag) (midi2cps 47.0))
          :| (gainT_' ("g1" <> tag) (gn t (pwfSub 1.0)) $ sinOsc_ ("s1" <> tag) (midi2cps 35.0))
          : (gainT_' ("g2" <> tag) (gn t (pwfH1 1.0)) $ sinOsc_ ("s2" <> tag) (midi2cps 59.0))
          : (gainT_' ("g3" <> tag) (gn t (pwfH2 1.0)) $ sinOsc_ ("s3" <> tag) (midi2cps 71.0))
          : Nil
      )
  )

scene :: Number -> Behavior (AudioUnit D1)
scene t =
  pure
    ( speaker'
        $ thunk t 0.5 "a"
    )

main :: Klank
main = klank { run = runInBrowser scene }
