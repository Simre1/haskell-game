{-# LANGUAGE TemplateHaskell #-}
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
import Control.Arrow
import Control.Applicative

import Sigma


-- data Input i (m :: * -> *) k where
--   GetInput :: Input i m i
--
-- makeSem ''Input

newtype SDLInput = SDLInput {rawInput :: [SDL.Event]}

runInputWithSignal :: Signal (Sem r) a i -> Signal (Sem (Input i : r)) a b -> Signal (Sem r) a b
runInputWithSignal calcNewI signal =
  let sig = signalMorph $ \sigStep (a,i) -> runConstInput i $ sigStep a
  in arr id &&& calcNewI >>> sig signal


runSDLEventInput :: Member (Lift IO) r => Signal (Sem (Input SDLInput : r)) a b -> Signal (Sem r) a b
runSDLEventInput = runInputWithSignal (arrAction $ SDLInput <$> SDL.pollEvents)


getKeyState :: Member (Input SDLInput) r => (SDL.Keysym -> Bool) -> Signal (Sem r) () (Maybe SDL.Keysym)
getKeyState filterKeysym = simpleFeedback empty $ arrM $ \currentKeyState -> do
    sdlEvents :: [SDL.EventPayload] <- fmap SDL.eventPayload . rawInput <$> input
    pure $ foldr foldKeysymState currentKeyState sdlEvents
  where
    foldKeysymState payload last = case payload of
        SDL.KeyboardEvent kData ->
          if filterKeysym (SDL.keyboardEventKeysym kData)
            then case SDL.keyboardEventKeyMotion kData of
                   SDL.Pressed -> pure (SDL.keyboardEventKeysym kData)
                   SDL.Released -> empty
            else last

        _ -> last
