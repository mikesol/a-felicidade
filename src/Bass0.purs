module Klank.Bass0 where

import Prelude
import Data.Array (head, last, span)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, gainT', runInBrowser, sinOsc, speaker')
import Math (pow)
import Type.Klank.Dev (Klank, klank)

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
