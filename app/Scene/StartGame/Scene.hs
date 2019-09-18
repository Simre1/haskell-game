{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Scene.StartGame.Scene where

import Polysemy (Members, Sem)
import Polysemy.Input (input, Input)
import Debug.Trace

import Sigma (Signal, liftAction)

import GameInput (GameInput(..))
import MyWindow (MyWindow)
import Scene.Scenes (Scene(..))

startGameScene :: Members [Input GameInput, MyWindow] r => Signal (Sem r) () -> Signal (Sem r) (Maybe Scene)
startGameScene render = (render *>) $ liftAction $ do
  GameInput _ _ _ go <- input
  pure $ if go then Just Level else Nothing
