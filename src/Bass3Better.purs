---- for some reason this has clearer audio than bass3bad?
module Klank.Bass3 where

import Prelude
import Data.Array (head, last, span)
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, gain, gainT_', runInBrowser, sinOscT_, sinOsc_, speaker')
import Math (pow)
import Type.Klank.Dev (Klank, klank)

kr = 20.0 / 1000.0 :: Number

pwf :: Number -> Array (Tuple Number Number)
pwf s =
  [ Tuple 0.0 0.0 ] <> (if s <= 0.0 then [] else [ Tuple s 0.0 ])
    <> [ Tuple (s + 0.024) 0.94
      , Tuple (s + 0.15) 0.5
      , Tuple (s + 0.66) 0.25
      , Tuple (s + 0.86) 0.0
      ]

pwfBend :: Number -> Number -> Array (Tuple Number Number)
pwfBend s bend =
  [ Tuple 0.0 47.0 ] <> (if s <= 0.0 then [] else [ Tuple s 47.0 ])
    <> [ Tuple (s + 0.4) 47.0, Tuple (s + 0.7) (47.0 + bend) ]

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
      ( (gainT_' ("g0" <> tag) (gn t (pwf 1.0)) $ sinOscT_ ("s0" <> tag) (midi2cps <$> (gn t (pwfBend 1.0 (1.5)))))
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