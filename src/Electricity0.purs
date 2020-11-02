module Klank.Electricity0 where

import Prelude
import Data.Array (head, last, span)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, audioWorkletGeneratorT, evalPiecewise, runInBrowser, speaker')
import Foreign.Object as O
import Math (pow)
import Type.Klank.Dev (Klank, Worklets, defaultEngineInfo, klank)

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

epwf = evalPiecewise kr

pwf :: Number -> Array (Tuple Number Number)
pwf s =
  [ Tuple 0.0 0.0 ] <> (if s <= 0.0 then [] else [ Tuple s 0.0 ])
    <> [ Tuple (s + 0.024) 0.94
      , Tuple (s + 0.15) 0.5
      , Tuple (s + 0.46) 0.1
      , Tuple (s + 0.76) 0.0
      ]

pwfElec :: Number -> Array (Tuple Number Number)
pwfElec s =
  [ Tuple 0.0 37.0 ] <> (if s <= 0.0 then [] else [ Tuple s 37.0 ])
    <> [ Tuple (s + 0.75) 25.0 ]

midi2cps :: Number -> Number
midi2cps n = (440.0 * (2.0 `pow` ((n - 69.0) / 12.0)))

scene :: Number -> Behavior (AudioUnit D1)
scene t =
  pure
    ( speaker'
        ( ( audioWorkletGeneratorT
              "klank-lf-noise"
              ( O.union
                  (O.singleton "gain" (epwf (pwf 0.2) t))
                  ( O.singleton "freq"
                      (epwf (pwfElec 0.2) t)
                  )
              )
          )
        )
    )

worklets :: Worklets
worklets _ res rej =
  ( do
      res [ "https://klank-share.s3.eu-west-1.amazonaws.com/K16025893847728133.js" ]
  )

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , worklets = worklets
    }
