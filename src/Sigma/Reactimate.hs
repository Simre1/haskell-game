module Sigma.Reactimate (reactimate, reactimateUntilTrue) where

import Sigma.Signal
import Control.Monad
import Control.Monad.IO.Class
import Polysemy

reactimateUntilTrue :: Signal (Sem r) Bool -> Sem r ()
reactimateUntilTrue signal = do
  (shouldClose,cont) <- stepSignal signal
  unless shouldClose $ reactimateUntilTrue cont

reactimate :: Signal (Sem r) () -> Sem r ()
reactimate signal = snd <$> stepSignal signal >>= reactimate
