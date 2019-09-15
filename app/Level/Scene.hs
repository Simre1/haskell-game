{-# LANGUAGE DataKinds #-}

module Level.Scene where

import Polysemy (Embed, Sem, Members)
import Polysemy.Input (Input)

import ECS.Apecs (ApecsSystem, executeApecsSystem)
import Sigma (Signal, liftAction, withInitialization)

import GameInput
import Level.Initialize.CollisionHandler (initializeCollisionHandlers)
import Level.Initialize.Player (initializePlayer)
import Level.Step.Delete (deleteOutOfBounds)
import Level.Step.Enemy (stepEnemies)
import Level.Step.Physics (stepPhysics)
import Level.Step.Player (stepPlayer)
import Level.Step.Scenario (scenarioSignal)
import Level.World (World)

levelSignal :: Members [Embed IO, ApecsSystem World, Input GameInput] r => Signal (Sem r) ()
levelSignal = withInitialization (executeApecsSystem $ initializeCollisionHandlers *> initializePlayer) $ \_ ->
  scenarioSignal *> liftAction (executeApecsSystem $ stepPlayer *> stepEnemies *> deleteOutOfBounds *> stepPhysics (1/60))
