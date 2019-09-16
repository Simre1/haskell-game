module Level.Step.Physics (stepPhysics) where

import Control.Monad.IO.Class (MonadIO)

import ECS.Apecs (SystemT)
import qualified ECS.Physics as Physics (stepPhysics)

import Level.World (World)

stepPhysics :: MonadIO m => SystemT World m ()
stepPhysics = Physics.stepPhysics (1/60)
