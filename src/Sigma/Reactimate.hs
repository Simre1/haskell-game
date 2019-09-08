module Sigma.Reactimate (reactimate, reactimateUntilTrue) where

import Sigma.Signal
import Control.Monad
import Control.Monad.IO.Class
import Polysemy

reactimateUntilTrue :: Monad m => Signal m Bool -> m ()
reactimateUntilTrue signal = do
  (shouldClose,cont) <- stepSignal signal
  unless shouldClose $ reactimateUntilTrue cont

reactimate :: Monad m => Signal m () -> m ()
reactimate signal = snd <$> stepSignal signal >>= reactimate
