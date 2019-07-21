module Sigma.Reactimate (reactimate, reactimateUntilTrue) where

import Sigma.Signal
import Control.Monad
import Control.Monad.IO.Class
import Polysemy

reactimateUntilTrue :: Signal r Bool -> Sem r ()
reactimateUntilTrue signal = do
  (shouldClose,cont) <- stepSignal signal
  unless shouldClose $ reactimateUntilTrue cont

reactimate :: Signal r () -> Sem r ()
reactimate signal = snd <$> stepSignal signal >>= reactimate
