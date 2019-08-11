{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Effect.Apecs where

import Polysemy
import Polysemy.State
import Control.Monad.IO.Class
import Sigma.Signal
import Data.IORef
import Apecs
import qualified Control.Monad.Reader as R
import Apecs.Physics
import Data.IORef
import Data.Maybe
import Control.Monad

data ApecsSystem w (m :: * -> *) a where
  ExecuteApecsSystem :: SystemT w m a -> ApecsSystem w m a
--  DoAtTheEnd :: SystemT w IO () -> ApecsSystem w m a

makeSem ''ApecsSystem

runApecsSystem :: w -> Sem (ApecsSystem w : r) a -> Sem r a
runApecsSystem w = interpretH $ \(ExecuteApecsSystem system) -> do
  m <- runT $ runSystem system w
  raise $ runApecsSystem w m

runApecs :: Sem r w -> Signal (Sem (ApecsSystem w : r)) a -> Signal (Sem r) a
runApecs initWorld signal = initWorld `withInitialization` \w ->
  signalMorph (runApecsSystem w) signal
