{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module SceneManager where

import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.Functor ((<$))
import Polysemy (Members, Sem, Embed)
import Polysemy.Input (Input)

import Sigma (Signal, switch)
import ECS.Apecs (runApecs)

import GameInput (GameInput)
import Level.Scene (levelSignal)
import Level.World (initWorld)
import MyWindow (MyWindow)
import Render.Level.All (renderLevel)
import Render.StartGame.All (renderStartGame)
import StartGame.Scene (startGameSignal)

data Scene = Level | StartGame

sceneManager :: Members [Embed IO, Input GameInput, MyWindow] r => Signal (Sem r) Bool
sceneManager = switch startGameScene switchScene *> pure False
  where switchScene Level = switch levelScene switchScene
        switchScene StartGame = switch startGameScene switchScene


levelScene :: Members [Embed IO, Input GameInput, MyWindow] r => Signal (Sem r) ((), Maybe Scene)
levelScene = runApecs (liftIO initWorld) $ ((),) <$> bool Nothing (Just StartGame) <$> levelSignal <* renderLevel

startGameScene :: Members [Input GameInput, MyWindow] r => Signal (Sem r) ((), Maybe Scene)
startGameScene = ((),) <$> bool Nothing (Just Level) <$> startGameSignal <* renderStartGame
