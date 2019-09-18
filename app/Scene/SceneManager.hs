{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scene.SceneManager where

import Polysemy (Members, Sem, Embed)
import Polysemy.Input (Input)

import Sigma (Signal, switch_)

import GameInput (GameInput)
import Scene.Level.Scene (levelScene)
import Scene.Scenes (Scene(..))
import Scene.StartGame.Scene (startGameScene)
import MyWindow (MyWindow)

import Render.Level.All (renderLevel)
import Render.StartGame.All (renderStartGame)

sceneManager :: forall r. Members [Embed IO, Input GameInput, MyWindow] r => Signal (Sem r) Bool
sceneManager = switch_ (startGameScene renderStartGame) switchScene *> pure False
  where
    switchScene :: Scene -> Signal (Sem r) ()
    switchScene Level = switch_ (levelScene renderLevel) switchScene
    switchScene StartGame = switch_ (startGameScene renderStartGame) switchScene
