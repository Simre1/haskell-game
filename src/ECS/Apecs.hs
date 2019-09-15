{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module ECS.Apecs
  ( ApecsSystem
  , executeApecsSystem
  , runApecsSystem
  , runApecs
  , module ApecsExport
  )
  where

import Polysemy (makeSem, interpretH, runT, raise, Sem)
import Apecs (runSystem, SystemT)
import Apecs as ApecsExport

import Sigma (withInitialization, signalMorph, Signal)

data ApecsSystem w (m :: * -> *) a where
  ExecuteApecsSystem :: SystemT w m a -> ApecsSystem w m a

makeSem ''ApecsSystem

runApecsSystem :: w -> Sem (ApecsSystem w : r) a -> Sem r a
runApecsSystem w = interpretH $ \(ExecuteApecsSystem system) -> do
  m <- runT $ runSystem system w
  raise $ runApecsSystem w m

runApecs :: Sem r w -> Signal (Sem (ApecsSystem w : r)) a -> Signal (Sem r) a
runApecs initWorld signal = initWorld `withInitialization` \w ->
  signalMorph (runApecsSystem w) signal
