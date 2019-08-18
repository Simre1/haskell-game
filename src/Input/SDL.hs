{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Input.SDL
  ( getKeyState
  , runSDLEventInput
  , SDLInput
  , rawInput
  ) where

import Data.Foldable
import qualified SDL
import Polysemy
import Polysemy.Input
import Polysemy.Reader
import Control.Applicative
import Debug.Trace
import Sigma.Signal
import Control.Monad.IO.Class
import Polysemy.State
import Aux.Polysemy.Input

newtype SDLInput = SDLInput {rawInput :: [SDL.Event]}
--
-- runInputWithSignal :: Signal (Sem r) i -> Signal (Sem (Input i : r)) b -> Signal (Sem r) b
-- runInputWithSignal calcNewI signal = readerSignal calcNewI $ signalMorph reinterpretInput signal
--   where reinterpretInput :: Sem (Input i : r) a -> Sem (Reader i : r) a
--         reinterpretInput = reinterpret (\Input -> ask)

runSDLEventInput :: Member (Embed IO) r => Signal (Sem (Input SDLInput : r)) b -> Signal (Sem r) b
runSDLEventInput = runInputWithSignal $ liftAction $ do
                    events <- SDL.pollEvents
                    pure $ SDLInput events



getKeyState :: Member (Input SDLInput) r => (SDL.Keysym -> Bool) -> Signal (Sem r) (Maybe SDL.Keysym)
getKeyState filterKeysym = feedback (Nothing :: Maybe SDL.Keysym) $ liftAction $ do
    currentKeyState <- get
    sdlEvents :: [SDL.EventPayload] <- fmap SDL.eventPayload . rawInput <$> input
    let keysym = foldl' foldKeysymState currentKeyState sdlEvents
    put keysym
    pure keysym

  where foldKeysymState last payload = case payload of
          SDL.KeyboardEvent kData ->
            if filterKeysym (SDL.keyboardEventKeysym kData)
              then
                case SDL.keyboardEventKeyMotion (kData) of
                     SDL.Pressed -> pure (SDL.keyboardEventKeysym kData)
                     SDL.Released -> Nothing
              else last

          _ -> last
