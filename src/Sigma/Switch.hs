module Sigma.Switch (switch) where

import Sigma.Signal

switch :: Signal r (b, Maybe c) -> (c -> Signal r b) -> Signal r b
switch s1 s2 = Signal $ do
  ((b,mC),cont) <- stepSignal s1
  maybe
    (pure $ (b, switch cont s2))
    (\c -> stepSignal (s2 c))
    mC
