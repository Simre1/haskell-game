{-# LANGUAGE UndecidableInstances #-}
module Effect.Input (Input, runInput, getInput) where

import Import
import Effect.SDLInput
import qualified SDL
import Control.Effect.Reader
import Sigma

import Data.Coerce

data Input i (m :: * -> *) k where
  GetInput :: (i -> k) -> Input i m k deriving Functor

instance HFunctor (Input i) where
  hmap _ = coerce

instance Effect (Input i) where
  handle state handler = coerce . fmap (handler . (<$ state))


newtype InputC i m a = InputC (ReaderC i m a) deriving (Monad, Applicative, Functor, MonadIO)

runInputC :: i -> (InputC i m a) -> m a
runInputC i (InputC reader) = runReader i reader

runInput :: (Member SDLInput sig, Carrier sig m) => i -> ([SDL.Event] -> i -> i) -> Signal (InputC i m) a b -> Signal m a b
runInput init f signal =
  let makeSig sig = Signal $ \(a,i) -> do
        newI <- f <$> sdlEvents <*> pure i
        (b, cont) <- runInputC newI $ stepSignal sig a
        pure ((b, newI), makeSig cont)
  in feedback init $ makeSig signal

instance (Carrier sig m, Effect sig, MonadIO m, Monad m) => Carrier (Input i :+: sig) (InputC i m) where
  eff = \case
          (L (GetInput feedInput)) -> InputC ask >>= feedInput
          a -> eff $ handleCoercible a

getInput :: (Member (Input i) sig, Carrier sig m) => m i
getInput = send $ GetInput pure
