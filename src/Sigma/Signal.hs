{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Sigma.Signal where

import Polysemy
import Polysemy.NonDet
import Polysemy.Reader
import Data.Functor ((<&>))
import Control.Arrow (second)
import Polysemy.State

data Signal r a = Signal {stepSignal :: Sem r (a, Signal r a)}

buildSignal = Signal

instance Functor (Signal r) where
  fmap f (Signal step) = Signal $ (\(a,cont) -> (f a, (f <$> cont))) <$> step

instance Applicative (Signal r) where
  pure a = buildSignal $ pure (a, pure a)
  (Signal step1) <*> (Signal step2) = buildSignal $ combine <$> step1 <*> step2
    where combine (v1, cont1) (v2, cont2) = (v1 v2, cont1 <*> cont2)

liftSem :: Sem r a -> Signal r a
liftSem sem = buildSignal $ (,liftSem sem) <$> sem

test :: Member (Lift Maybe) r => Sem r Int
test = sendM Nothing

printMaybe :: Show a => Maybe a -> IO ()
printMaybe = print

feedback :: s -> Signal (State s : r) a -> Signal r a
feedback initial signal = buildSignal $ do
  (newState, (a, cont)) <- runState initial $ stepSignal signal
  pure (a, feedback newState cont)

signalMorph :: (forall c . (Sem r1 (a1, c)) -> (Sem r2 (a2, c))) -> Signal r1 a1 -> Signal r2 a2
signalMorph f signal = Signal $ fmap (second (signalMorph f)) $ f (stepSignal signal)
{-# INLINABLE signalMorph #-}


readerSignal :: Signal r a -> Signal (Reader a : r) b -> Signal r b
readerSignal sig sig2 = Signal $ do
  (a, cont1) <- stepSignal sig
  (b, cont2) <- runReader a (stepSignal sig2)
  pure (b, readerSignal cont1 cont2)

signalAsk :: Member (Reader a) r => Signal r a
signalAsk = liftSem ask

stateSignal :: Signal r s -> Signal (State s : r) b -> Signal r (s, b)
stateSignal sig sig2 = Signal $ do
  (a, cont1) <- stepSignal sig
  (s, (b, cont2)) <- runState a (stepSignal sig2)
  pure ((s,b), stateSignal cont1 cont2)


signalGet :: Member (State s) r => Signal r s
signalGet = liftSem get

signalPut :: Member (State s) r => Signal r s -> Signal r ()
signalPut signal = buildSignal $ do
  (s, cont) <- stepSignal signal
  put s
  pure ((), signalPut cont)

signalModify :: Member (State s) r => Signal r (s -> s) -> Signal r ()
signalModify signal = buildSignal $ do
  (f, cont) <- stepSignal signal
  modify f
  pure ((), signalModify cont)

doOnce :: Sem r a -> Signal r a
doOnce action = Signal $ do
  a <- action
  pure (a, pure a)

withInitialization :: Sem r x -> (x -> Signal r a) -> Signal r a
withInitialization initialize f = buildSignal $ do
  x <- initialize
  stepSignal (f x)
