{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module SceneManager where

import Control.Monad.IO.Class (liftIO)
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

data Scene = Level

sceneManager :: Members [Embed IO, Input GameInput, MyWindow] r => Signal (Sem r) Bool
sceneManager = switch levelScene switchScene *> pure False
  where switchScene Level = switch levelScene switchScene


levelScene :: Members [Embed IO, Input GameInput, MyWindow] r => Signal (Sem r) ((), Maybe Scene)
levelScene = runApecs (liftIO initWorld) $ ((),Nothing) <$ (levelSignal *> renderLevel)
