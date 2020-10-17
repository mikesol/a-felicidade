module A.Felicidade.Pluck where

import Prelude
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, audioWorkletGenerator, g'add, g'delay, g'gain, graph, runInBrowser, speaker')
import Foreign.Object as O
import Record.Extra (SLProxy(..), SNil)
import Type.Data.Graph (type (:/))
import Type.Klank.Dev (Klank, Worklets, klank)

worklets :: Worklets
worklets _ res rej =
  ( do
      res [ "https://klank-share.s3.eu-west-1.amazonaws.com/K16029410960343210.js" ]
  )

scene :: Number -> Behavior (AudioUnit D1)
scene time =
  pure
    $ speaker'
        ( graph
            { aggregators:
                { out: Tuple g'add (SLProxy :: SLProxy ("combine" :/ SNil))
                , combine: Tuple g'add (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                , gain: Tuple (g'gain 0.9) (SLProxy :: SLProxy ("del" :/ SNil))
                }
            , processors:
                { del: Tuple (g'delay 0.005) (SProxy :: SProxy "combine")
                }
            , generators:
                { mic:
                    audioWorkletGenerator
                      "klank-lf-burst"
                      (O.union (O.singleton "nsamples" 15.0) (O.singleton "freq" 0.3))
                }
            }
        )

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , worklets = worklets
    }
