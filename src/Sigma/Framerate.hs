module Sigma.Framerate (limitFramerate) where


import GHC.Clock
import Sigma.Signal
import Control.Concurrent
import System.Mem
import Data.IORef
import Control.Monad.IO.Class
import Polysemy


limitFramerate :: Member (Embed IO) r => Int -> Signal (Sem r) b -> Signal (Sem r) b
limitFramerate fps signal = Signal $ do
  t <- liftIO getMonotonicTimeNSec
  d <- pure $ toEnum (1000000000 `quot` fps)
  let newSig = Signal $ do
        (b, cont) <- stepSignal signal
        return (b, cont)
  stepSignal (makeSignal d (t-d) signal)

  where makeSignal d t1 s = Signal $ do
          (b, cont) <-  id $! (stepSignal s)
          t2 <- liftIO getMonotonicTimeNSec
          let diff = t2 - t1
          let waitTime = if diff >= d
                then 0
                else d - diff
          liftIO $ threadDelay $ fromEnum waitTime `quot` 1000
          pure (b, makeSignal d (t2 + waitTime) cont)
