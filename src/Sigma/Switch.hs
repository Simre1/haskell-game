module Sigma.Switch (switch, switch_) where

import Sigma.Signal

switch :: Monad m => Signal m (b, Maybe c) -> (c -> Signal m b) -> Signal m b
switch s1 s2 = Signal $ do
  ((b,mC),cont) <- stepSignal s1
  maybe
    (pure $ (b, switch cont s2))
    (\c -> stepSignal (s2 c))
    mC

switch_ :: Monad m => Signal m (Maybe c) -> (c -> Signal m ()) -> Signal m ()
switch_ s1 s2 = Signal $ do
  (mC,cont) <- stepSignal s1
  maybe
    (pure $ ((), switch_ cont s2))
    (\c -> stepSignal (s2 c))
    mC
