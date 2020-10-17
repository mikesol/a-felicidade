module A.Felicidade.IdentitySanityCheck where

import Prelude
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, audioWorkletGenerator, g'add, g'audioWorkletProcessor, g'gain, graph, microphone, runInBrowser, sinOsc, speaker')
import Foreign.Object as O
import Record.Extra (SLProxy(..), SNil)
import Type.Data.Graph (type (:/))
import Type.Klank.Dev (Klank, Worklets, klank)

worklets :: Worklets
worklets _ res rej =
  ( do
      res
        [ "https://klank-share.s3.eu-west-1.amazonaws.com/K16029477644118762.js"
        ]
  )

scene :: Number -> Behavior (AudioUnit D1)
scene time =
  pure
    $ speaker'
        ( graph
            { aggregators:
                { out: Tuple g'add (SLProxy :: SLProxy ("del" :/ "foo" :/ SNil))
                }
            , processors:
                { del:
                    Tuple
                      ( g'audioWorkletProcessor
                          "klank-micro-delay"
                          ( O.singleton
                              "len"
                              50.0
                          )
                      )
                      (SProxy :: SProxy "mic")
                , foo:
                    Tuple
                      ( g'audioWorkletProcessor
                          "klank-micro-delay"
                          ( O.singleton
                              "len"
                              950.0
                          )
                      )
                      (SProxy :: SProxy "mic")
                }
            , generators:
                { mic:
                    microphone
                }
            }
        )

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , worklets = worklets
    , enableMicrophone = true
    }
