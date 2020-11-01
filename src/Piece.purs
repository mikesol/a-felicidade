module A.Felicidade where

import Prelude
import Control.Parallel (parallel, sequential)
import Control.Promise (toAffE)
import Data.Array (filter, foldl, mapWithIndex, fold, head, last, range, span)
import Data.Either (either)
import Data.Int (toNumber)
import Data.Lazy (Lazy, defer, force)
import Data.Lens (_1, _2, over, traversed)
import Data.List ((:), List(..))
import Data.Map as M
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Set (isEmpty)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (class Pos, D2)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, try)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioParameter(..), AudioUnit, BrowserAudioBuffer, EngineInfo, IAudioUnit(..), bandpassT_, convolver_, decodeAudioDataFromUri, defaultExporter, dynamicsCompressor_, gain, gainT_, gainT_', gain_, gain_', highpassT_, highpass_, pannerMonoT_, pannerMono_, pannerT_, panner_, playBuf, playBufT_, playBufWithOffset_, playBuf_, runInBrowser, runInBrowser_, sinOsc_, speaker, speaker')
import FRP.Behavior.Mouse (buttons, position)
import FRP.Event.Mouse (Mouse, getMouse)
import Foreign.Object as O
import Math (abs, cos, pi, pow, sin)
import Type.Klank.Dev (Klank, Klank', affable, defaultEngineInfo, klank)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth, outerHeight, outerWidth)

----
-- util
--
loopDownload :: AudioContext -> String -> Aff BrowserAudioBuffer
loopDownload ctx str =
  res
    >>= either
        ( \e -> do
            -- liftEffect $ log (show e)
            -- a bit buggy, but it gets the job done...
            delay (Milliseconds 20.0)
            loopDownload ctx str
        )
        pure
  where
  res = try $ toAffE (decodeAudioDataFromUri ctx str)

makeBuffersUsingCacheWithPersistentDownloads :: (O.Object BrowserAudioBuffer -> Tuple (Array (Tuple String String)) (O.Object BrowserAudioBuffer)) -> AudioContext -> O.Object BrowserAudioBuffer -> (O.Object BrowserAudioBuffer -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
makeBuffersUsingCacheWithPersistentDownloads bf ctx prev' =
  affable do
    sequential
      ( O.union <$> (pure prev)
          <*> ( sequence
                $ O.fromFoldable
                    ( map
                        ( over _2
                            (parallel <<< loopDownload ctx)
                        )
                        (filter (not <<< flip O.member prev <<< fst) newB)
                    )
            )
      )
  where
  (Tuple newB prev) = bf prev'

makeBuffersKeepingCacheWithPersistentDownloads :: Array (Tuple String String) -> AudioContext -> O.Object BrowserAudioBuffer -> (O.Object BrowserAudioBuffer -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
makeBuffersKeepingCacheWithPersistentDownloads = makeBuffersUsingCacheWithPersistentDownloads <<< Tuple

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

conv440 :: Int -> Number
conv440 i = 440.0 * (2.0 `pow` ((toNumber $ 0 + i) / 12.0))

boundPlayer :: Number -> Number -> Lazy (List (AudioUnit D2)) -> List (AudioUnit D2)
boundPlayer len time a = if (time) + kr >= 0.0 && time < (len) then force a else Nil

aFelicidadeEngineInfo = defaultEngineInfo :: EngineInfo

kr = (toNumber aFelicidadeEngineInfo.msBetweenSamples) / 1000.0 :: Number

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter Number
epwf p s =
  let
    ht = span ((s >= _) <<< fst) p

    left = fromMaybe (Tuple 0.0 0.0) $ last ht.init

    right =
      fromMaybe
        (maybe (Tuple 10000.0 0.0) (over _1 (_ + 1.0)) $ last p)
        $ head ht.rest
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

fromCloud :: String -> String
fromCloud s = "https://klank-share.s3-eu-west-1.amazonaws.com/a-felicidade/Samples/" <> s

type AFelicidadeAccumulator
  = { isClicked :: Boolean
    }

type AFelicidadeInteractiveInfo
  = { isClicked :: Boolean
    , justClicked :: Boolean
    , pos :: Maybe { x :: Number, y :: Number }
    }

scene ::
  Number ->
  Number ->
  Mouse ->
  AFelicidadeAccumulator ->
  Number ->
  Behavior (IAudioUnit D2 AFelicidadeAccumulator)
scene w h mouse acc time = f <$> pos' <*> isClicked'
  where
  f pos isClicked =
    ( IAudioUnit
        ( speaker'
            ( gainT_ "masterFader"
                ((epwf [ Tuple 0.0 1.0, Tuple 1000.0 1.0 ]) time)
                ( zero
                    :| fold
                        ( ( map
                              ( \fn ->
                                  fn time
                                    { isClicked
                                    , justClicked
                                    , pos
                                    }
                              )
                              (eventList)
                          )
                        )
                )
            )
        )
        ( acc
            { isClicked = isClicked
            }
        )
    )
    where
    justClicked = (not acc.isClicked && isClicked)

  pos' :: Behavior (Maybe { x :: Number, y :: Number })
  pos' =
    (map <<< map)
      ( \{ x, y } ->
          { x: toNumber x, y: toNumber y }
      )
      (position mouse)

  isClicked' :: Behavior Boolean
  isClicked' = map (not <<< isEmpty) $ buttons mouse

----
--
main :: Klank' AFelicidadeAccumulator
main =
  klank
    { buffers =
      makeBuffersKeepingCacheWithPersistentDownloads
        ( map (\s -> Tuple s (fromCloud s))
            [ "Afoxe/Afoxe.ogg"
            , "Afoxe/Agogo_2.ogg"
            , "Capoeira/Pandeiro_1.ogg"
            , "Frevo/Surdo_2D.ogg"
            ]
        )
    , accumulator = \res rej -> res { isClicked: false }
    , run =
      runInBrowser_ do
        mouse <- getMouse
        w <- window
        width <- innerWidth w
        height <- innerHeight w
        pure (scene (toNumber width) (toNumber height) mouse)
    , exporter = defaultExporter
    }

---------------------------------------------
------------------------------------
------------- stuff
simplPlyr :: String -> Number -> AFelicidadeInteractiveInfo -> List (AudioUnit D2)
simplPlyr s time info =
  boundPlayer 20.0 time
    ( defer \_ ->
        pure
          $ (playBuf_ (s) (s) 1.0)
    )

eventList :: Array (Number -> AFelicidadeInteractiveInfo -> List (AudioUnit D2))
eventList =
  [ atT 1.0 $ (simplPlyr "Afoxe/Afoxe.ogg")
  , atT 1.0 $ (simplPlyr "Afoxe/Agogo_2.ogg")
  , atT 1.0 $ (simplPlyr "Capoeira/Pandeiro_1.ogg")
  , atT 1.0 $ (simplPlyr "Frevo/Surdo_2D.ogg")
  ]
