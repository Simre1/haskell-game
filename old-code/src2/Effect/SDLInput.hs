{-# LANGUAGE UndecidableInstances #-}

module Effect.SDLInput (SDLInput, sdlEvents, runSDLInput) where

import qualified SDL as SDL
import Control.Effect.Reader

import Import
import Sigma

import Data.Coerce

data SDLInput (m :: * -> *) k where
  SDLInput :: (SDLInputData -> k) -> SDLInput m k deriving Functor

instance HFunctor SDLInput where
  hmap _ = coerce

instance Effect SDLInput where
  handle state handler = coerce . fmap (handler . (<$ state))


newtype SDLInputC m a = SDLInputC (ReaderC SDLInputData m a) deriving (Monad, Applicative, Functor, MonadIO)


instance (Carrier sig m, Effect sig, MonadIO m, Monad m) => Carrier (SDLInput :+: sig) (SDLInputC m) where
  eff = \case
          (L (SDLInput feedInput)) -> SDLInputC ask >>= feedInput
          --a -> eff $ handleCoercible a
          (R a) -> SDLInputC $ eff $ R $ handleCoercible a

newtype SDLInputData = SDLInputData {events :: [SDL.Event]}

runSDLInputC :: SDLInputData -> SDLInputC m a -> m a
runSDLInputC inputData (SDLInputC reader) = runReaderC reader inputData

runSDLInput :: MonadIO m => Signal (SDLInputC m) a b -> Signal m a b
runSDLInput signal = Signal $ \a -> do
  events <- SDL.pollEvents
  (b, cont) <- runSDLInputC (SDLInputData events) $ stepSignal signal a
  return (b, runSDLInput cont)

sdlEvents :: (Member SDLInput sig, Carrier sig m) => m [SDL.Event]
sdlEvents = send $ SDLInput (pure . events)
