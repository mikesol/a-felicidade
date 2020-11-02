module Klank.Bass0 where

import Prelude
import Data.Array (head, last, span)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, evalPiecewise, gainT', runInBrowser, sinOsc, speaker')
import Math (pow)
import Type.Klank.Dev (Klank, defaultEngineInfo, klank)

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

midi2cps :: Number -> Number
midi2cps n = (440.0 * (2.0 `pow` ((n - 69.0) / 12.0)))

scene :: Number -> Behavior (AudioUnit D1)
scene t = pure (speaker' (gainT' (epwf (pwf 0.2) t) $ sinOsc (midi2cps 25.0)))

main :: Klank
main = klank { run = runInBrowser scene }
