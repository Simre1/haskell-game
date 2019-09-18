{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Scene.Level.Scene where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Polysemy (Embed, Sem, Members)
import Polysemy.Input (Input)

import ECS.Apecs (ApecsSystem, executeApecsSystem, runApecs)
import Sigma (Signal, liftAction, withInitialization)

import GameInput
import MyWindow (MyWindow)
import Scene.Level.Initialize.CollisionHandler (initializeCollisionHandlers)
import Scene.Level.Initialize.Player (initializePlayer)
import Scene.Level.Step.Delete (deleteOutOfBounds)
import Scene.Level.Step.Enemy (stepEnemies)
import Scene.Level.Step.Physics (stepPhysics)
import Scene.Level.Step.Player (stepPlayer)
import Scene.Level.Step.Scenario (scenarioSignal, level1Waves)
import Scene.Level.World (World, initWorld)
import Scene.Level.WorldAccessors (isPlayerAlive)
import Scene.Scenes (Scene(..))


levelScene :: Members [Embed IO, Input GameInput, MyWindow] r => Signal (Sem (ApecsSystem World : r)) () -> Signal (Sem r) (Maybe Scene)
levelScene render = runApecs (liftIO initWorld) $ withInitialization (executeApecsSystem $ initializeCollisionHandlers *> initializePlayer) $ \_ ->
  const <$> step <*> render
  where
    step :: Members [Embed IO, Input GameInput, ApecsSystem World] r => Signal (Sem r) (Maybe Scene)
    step = (<|>) <$> scenarioSignal (level1Waves, StartGame) <*> liftAction (executeApecsSystem $ stepPlayer *> stepEnemies *> deleteOutOfBounds *> stepPhysics *> (bool (Just StartGame) Nothing <$> isPlayerAlive))
