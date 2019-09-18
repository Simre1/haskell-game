module Scene.Level.Step.Enemy where

import Control.Monad.IO.Class (MonadIO)
import Linear.V2 (V2(..))

import ECS.Apecs (SystemT, cmapM, cmapM)
import ECS.Physics (Position(..), Velocity(..))

import Scene.Level.Spawn.Bullet
import Scene.Level.World (World, Enemy(..), EnemyType(..))

stepEnemies :: MonadIO m => SystemT World m ()
stepEnemies = do
  cmapM $ \(Enemy enemyType, Position pos) -> do
    case enemyType of
      Argo cooldown speed ->
        if cooldown < 1
          then spawnStraightBullet (Position pos) (Velocity $ V2 0 (-300)) False *> pure (Enemy (Argo ((5 - fromEnum speed) * 60) speed))
          else pure (Enemy $ Argo (pred cooldown) speed)
      otherEnemy -> pure (Enemy otherEnemy)
