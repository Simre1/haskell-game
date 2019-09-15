{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}


module Sigma.Signal where

import Control.Arrow (second)
import Data.Functor ((<&>))
import Polysemy (interpret, Member, Sem)
import Polysemy.NonDet (NonDet)
import Polysemy.Reader (Reader, runReader, ask)
import Polysemy.State (State, get, put, modify, runState)

data Signal m a = Signal {stepSignal :: m (a, Signal m a)}

buildSignal = Signal

instance Functor m => Functor (Signal m) where
  fmap f (Signal step) = Signal $ (\(a,cont) -> (f a, (f <$> cont))) <$> step

instance Applicative m => Applicative (Signal m) where
  pure a = buildSignal $ pure (a, pure a)
  (Signal step1) <*> (Signal step2) = buildSignal $ combine <$> step1 <*> step2
    where combine (v1, cont1) (v2, cont2) = (v1 v2, cont1 <*> cont2)

liftAction :: Functor m => m a -> Signal m a
liftAction sem = buildSignal $ (,liftAction sem) <$> sem

feedback :: s -> Signal (Sem (State s : r)) a -> Signal (Sem r) a
feedback initial signal = buildSignal $ do
  (newState, (a, cont)) <- runState initial $ stepSignal signal
  pure (a, feedback newState cont)

signalMorph :: Functor m2 => (forall c . (m1 (a1, c)) -> (m2 (a2, c))) -> Signal m1 a1 -> Signal m2 a2
signalMorph f signal = Signal $ fmap (second (signalMorph f)) $ f (stepSignal signal)
{-# INLINABLE signalMorph #-}


readerSignal :: Signal (Sem r) a -> Signal (Sem (Reader a : r)) b -> Signal (Sem r) b
readerSignal sig sig2 = Signal $ do
  (a, cont1) <- stepSignal sig
  (b, cont2) <- runReader a (stepSignal sig2)
  pure (b, readerSignal cont1 cont2)

signalAsk :: Member (Reader a) r => Signal (Sem r) a
signalAsk = liftAction ask

stateSignal :: Signal (Sem r) s -> Signal (Sem (State s : r)) b -> Signal (Sem r) (s, b)
stateSignal sig sig2 = Signal $ do
  (a, cont1) <- stepSignal sig
  (s, (b, cont2)) <- runState a (stepSignal sig2)
  pure ((s,b), stateSignal cont1 cont2)


signalGet :: Member (State s) r => Signal (Sem r) s
signalGet = liftAction get

signalPut :: Member (State s) r => Signal (Sem r) s -> Signal (Sem r) ()
signalPut signal = buildSignal $ do
  (s, cont) <- stepSignal signal
  put s
  pure ((), signalPut cont)

signalModify :: Member (State s) r => Signal (Sem r) (s -> s) -> Signal (Sem r) ()
signalModify signal = buildSignal $ do
  (f, cont) <- stepSignal signal
  modify f
  pure ((), signalModify cont)

doOnce :: Monad m => m a -> Signal m a
doOnce action = Signal $ do
  a <- action
  pure (a, pure a)

withInitialization :: Monad m => m x -> (x -> Signal m a) -> Signal m a
withInitialization initialize f = buildSignal $ do
  x <- initialize
  stepSignal (f x)
