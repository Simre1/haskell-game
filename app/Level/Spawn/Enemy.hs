module Level.Spawn.Enemy where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Linear.V2 (V2(..))


import ECS.Physics (Body(..), Shape(..), Mass(..), CollisionFilter(..), Velocity(..), Position(..), CollisionType(..), maskList, cRectangle)
import ECS.Apecs (SystemT, newEntity)

import Level.World (EnemyType(..), Enemy(..), World)


spawnEnemy :: MonadIO m => EnemyType -> V2 Double -> SystemT World m ()
spawnEnemy enemyType pos =  void $ case enemyType of
  (Argo t) -> do
    argo <- newEntity (Enemy $ Argo t, DynamicBody, Position pos, Velocity $ V2 0 (-50))
    newEntity (Shape argo (cRectangle $ V2 40 40), Mass 100, enemyCollisionOptions)
  (Runex speed path) -> do
    runex <- newEntity (Enemy $ Runex speed path, DynamicBody, Position pos)
    newEntity (Shape runex (cRectangle $ V2 40 40), Mass 100, enemyCollisionOptions)
  where enemyCollisionOptions = (CollisionFilter 2 (maskList [2]) (maskList [3]), CollisionType 2)
