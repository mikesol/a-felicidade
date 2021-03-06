---- adds a pitch bend
---- processing in browser pretty slow on Windows
---- maybe not possible in current form
module Klank.Bass3Bad where

import Prelude
import Data.Array (head, last, span)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, evalPiecewise, gain, gainT_', runInBrowser, sinOscT_, sinOsc_, speaker)
import Math (pow)
import Type.Klank.Dev (Klank, defaultEngineInfo, klank)

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

epwf = evalPiecewise kr

pwf :: Number -> Array (Tuple Number Number)
pwf s =
  [ Tuple 0.0 0.0 ] <> (if s <= 0.0 then [] else [ Tuple s 0.0 ])
    <> [ Tuple (s + 0.024) 0.94
      , Tuple (s + 0.15) 0.5
      , Tuple (s + 0.66) 0.25
      , Tuple (s + 0.86) 0.0
      ]

pwfBend :: Number -> Number -> Number -> Array (Tuple Number Number)
pwfBend s pitch bend =
  [ Tuple 0.0 pitch ] <> (if s <= 0.0 then [] else [ Tuple s pitch ])
    <> [ Tuple (s + 0.4) pitch, Tuple (s + 0.7) (pitch + bend) ]

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

midi2cps :: Number -> Number
midi2cps n = (440.0 * (2.0 `pow` ((n - 69.0) / 12.0)))

thunk :: Number → Number → Number -> Number -> Number -> String → (List (AudioUnit D1))
thunk t gg pitch os ud tag =
  ( if t - os > (-0.1) && t - os < 1.6 then
      pure
        $ gain gg
            ( (gainT_' ("g0" <> tag) (epwf (pwf os) t) $ sinOsc_ ("s0" <> tag) (midi2cps (epwf (pwfBend os (pitch + 12.0) ud) t).param))
                :| (gainT_' ("g1" <> tag) (epwf (pwfSub os) t) $ sinOsc_ ("s1" <> tag) (midi2cps pitch))
                : (gainT_' ("g2" <> tag) (epwf (pwfH1 os) t) $ sinOsc_ ("s2" <> tag) (midi2cps (pitch + 24.0)))
                : (gainT_' ("g3" <> tag) (epwf (pwfH2 os) t) $ sinOsc_ ("s3" <> tag) (midi2cps (pitch + 36.0)))
                : Nil
            )
    else
      Nil
  )

tempo = 60.0 / 96.0 :: Number

scene :: Number -> Behavior (AudioUnit D1)
scene t =
  pure
    ( speaker
        $ zero
        :| ( thunk t 0.3 35.0 1.0 1.5 "a"
              <> thunk t 0.3 30.0 (1.0 + tempo) (-0.6) "b"
              <> thunk t 0.3 35.0 (1.0 + (tempo * 2.0)) 1.5 "a"
              <> thunk t 0.3 30.0 (1.0 + (tempo * 3.0)) (-0.6) "a"
              <> Nil
          )
    )

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , engineInfo =
      defaultEngineInfo
        { rewindUpperBound = 0.5
        }
    }
