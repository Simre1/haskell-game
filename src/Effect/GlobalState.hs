{-# LANGUAGE TemplateHaskell #-}

module Effect.GlobalState (GlobalState, globalStateGet, globalStatePut, withGlobalState, runGlobalStateWithIORef, withGlobalStateSignal) where

import Polysemy
import Polysemy.State
import Control.Monad.IO.Class
import Sigma.Signal
import Data.IORef


data GlobalState a (m :: * -> *) k where
  GlobalStateGet :: GlobalState a m a
  GlobalStatePut :: a -> GlobalState a m ()

makeSem ''GlobalState


runGlobalStateE :: Member (Lift IO) r => IO s -> (s -> IO ()) -> Sem (GlobalState s : r) a -> Sem r a
runGlobalStateE getState putState = interpret $ \case
  (GlobalStateGet) -> liftIO getState
  (GlobalStatePut value) -> liftIO (putState value)

runGlobalStateWithIORef :: Member (Lift IO) r => a -> Signal (GlobalState a : r) b -> Signal r b
runGlobalStateWithIORef s signal = withInitialization (liftIO (newIORef s)) $ \ref ->
  signalMorph (runGlobalStateE (readIORef ref) (writeIORef ref)) signal

withGlobalState :: Member (GlobalState s) r => Sem (State s : r) a -> Sem r a
withGlobalState sem = do
  initialState <- globalStateGet
  (newState, a) <- runState initialState sem
  globalStatePut newState
  pure a

withGlobalStateSignal :: Member (GlobalState s) r => Signal (State s : r) a -> Signal r a
withGlobalStateSignal = signalMorph withGlobalState
