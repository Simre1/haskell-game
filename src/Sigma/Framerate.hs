module Sigma.Framerate (limitFramerate) where


import GHC.Clock
import Sigma.Signal
import Control.Concurrent
import System.Mem
import Data.IORef
import Control.Monad.IO.Class

limitFramerate :: MonadIO m => Int -> Signal m a b -> Signal m a b
limitFramerate fps signal = Signal $ \a -> do
  t <- liftIO getMonotonicTimeNSec
  d <- pure $ toEnum (1000000000 `quot` fps)
  let newSig = Signal $ \a -> do
        (b, cont) <- stepSignal signal a
        return (b, cont)
  stepSignal (makeSignal d (t-d) signal) a

  where makeSignal d t1 s = Signal $ \a -> do
          (b, cont) <- (stepSignal s $!) a
          t2 <- liftIO getMonotonicTimeNSec
          let diff = t2 - t1
          let waitTime = if diff >= d
                then 0
                else d - diff
          liftIO $ threadDelay $ fromEnum waitTime `quot` 1000
          pure (b, makeSignal d (t2 + waitTime) cont)
