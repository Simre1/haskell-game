{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Sigma.Framerate (limitFramerate) where

import Sigma.Signal (Signal, buildSignal, stepSignal)
import GHC.Clock (getMonotonicTimeNSec)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO, MonadIO)


limitFramerate :: MonadIO m => Int -> Signal m b -> Signal m b
limitFramerate fps signal = buildSignal $ do
  t <- liftIO getMonotonicTimeNSec
  d <- pure $ toEnum (1000000000 `quot` fps)
  let newSig = buildSignal $ do
        (b, cont) <- stepSignal signal
        return (b, cont)
  stepSignal (makeSignal d (t-d) signal)

  where makeSignal d t1 s = buildSignal $ do
          (!b, cont) <- stepSignal s
          t2 <- liftIO getMonotonicTimeNSec
          let diff = t2 - t1
          let waitTime = if diff >= d
                then 0
                else d - diff
          liftIO $ threadDelay $ fromEnum waitTime `quot` 1000
          pure (b, makeSignal d (t2 + waitTime) cont)
