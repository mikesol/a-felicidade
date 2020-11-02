---- was trying to create dubstep warble, got a REALLY interesting
---- chirp. happened because I doubled up on midi2cps below
---- resulted in reaaalllly high sound!
module Klank.HappyAccident0 where

import Prelude
import Data.Array (head, last, range, span)
import Data.Array as A
import Data.Int (ceil, toNumber)
import Data.Lens (_2, over, traversed)
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Data.NonEmpty ((:|))
import Data.Set as DS
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, evalPiecewise, gain, gainT_', runInBrowser, sinOscT_, sinOsc_, speaker')
import Math (pow, sin, pi)
import Type.Klank.Dev (Klank, defaultEngineInfo, klank)

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

epwf = evalPiecewise kr

opPwf :: (Number -> Number -> Number) -> Array (Tuple Number Number) -> Array (Tuple Number Number) -> Array (Tuple Number Number)
opPwf op l r =
  ( \i ->
      Tuple i
        (op (getY i l) (getY i r))
  )
    <$> A.sort (A.fromFoldable (DS.fromFoldable $ fst <$> (l <> r)))

addPwf = opPwf (+) :: Array (Tuple Number Number) -> Array (Tuple Number Number) -> Array (Tuple Number Number)

mulPwf = opPwf (*) :: Array (Tuple Number Number) -> Array (Tuple Number Number) -> Array (Tuple Number Number)

f2pwf :: Number -> Number -> Number -> (Number -> Number) -> Array (Tuple Number Number)
f2pwf s e q f =
  (\i -> let x = (toNumber i) * q + s in Tuple x (f x))
    <$> (range 0 $ ceil ((e - s) / q))

getY :: Number → Array (Tuple Number Number) → Number
getY s p =
  let
    ht = span ((s >= _) <<< fst) p

    left = fromMaybe (Tuple 0.0 0.0) $ last ht.init

    right = fromMaybe (Tuple 101.0 0.0) $ head ht.rest

    m = (snd right - snd left) / (fst right - fst left)

    b = (snd right - (m * fst right))
  in
    m * s + b

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

pwfRamp :: Number -> Array (Tuple Number Number)
pwfRamp s =
  [ Tuple 0.0 0.0 ] <> (if s <= 0.0 then [] else [ Tuple s 0.0 ])
    <> [ Tuple (s + 1.0) 1.0 ]

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

bend'dub :: Number -> Number -> Array (Tuple Number Number)
bend'dub s b =
  addPwf
    (mulPwf (f2pwf 0.0 2.0 0.02 (\x -> 30.0 * sin (4.0 * pi * x))) (pwfRamp s))
    (over (traversed <<< _2) midi2cps (pwfBend s b))

thunk :: Number → Number → String → AudioUnit D1
thunk t gg tag =
  ( gain gg
      ( (gainT_' ("g0" <> tag) (epwf (pwf 1.0) t) $ sinOsc_ ("s0" <> tag) (midi2cps (epwf (bend'dub 1.0 2.0) t).param))
          :| (gainT_' ("g1" <> tag) (epwf (pwfSub 1.0) t) $ sinOsc_ ("s1" <> tag) (midi2cps 35.0))
          : (gainT_' ("g2" <> tag) (epwf (pwfH1 1.0) t) $ sinOsc_ ("s2" <> tag) (midi2cps 59.0))
          : (gainT_' ("g3" <> tag) (epwf (pwfH2 1.0) t) $ sinOsc_ ("s3" <> tag) (midi2cps 71.0))
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
