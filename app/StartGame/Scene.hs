{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module StartGame.Scene where

import Polysemy (Member, Sem)
import Polysemy.Input (input, Input)
import Debug.Trace

import Sigma (Signal, liftAction)

import GameInput (GameInput(..))

startGameSignal :: Member (Input GameInput) r => Signal (Sem r) Bool
startGameSignal = liftAction $ do
  GameInput _ _ _ go <- input
  pure $ go
