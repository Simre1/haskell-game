module Sigma.Switch (switch) where

import Sigma.Signal

switch :: Monad m => Signal m a (b, Maybe c) -> (c -> Signal m a b) -> Signal m a b
switch s1 s2 = Signal $ \a -> do
  ((b,mC),cont) <- stepSignal s1 a
  maybe
    (return $ (b, switch cont s2))
    (\c -> stepSignal (s2 c) a)
    mC
