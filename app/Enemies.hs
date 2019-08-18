module Enemies where

import Effect.Apecs
import Apecs.Physics
import Control.Monad.IO.Class
import Control.Monad
import Polysemy as P
import Polysemy.Input
import Polysemy.Reader as R
import Sigma.Signal
import Control.Monad
import Debug.Trace

import Bullets
import World

spawnEnemy :: MonadIO m => EnemyType -> V2 Double -> SystemT World m ()
spawnEnemy enemyType pos =  void $ case enemyType of
  (Argo t) -> do
    argo <- newEntity (Enemy $ Argo t, DynamicBody, Position pos, Velocity $ V2 0 (-50))
    newEntity (Shape argo (cRectangle $ V2 40 40), Mass 100, enemyCollisionOptions)
  (Runex speed path) -> do
    runex <- newEntity (Enemy $ Runex speed path, DynamicBody, Position pos)
    newEntity (Shape runex (cRectangle $ V2 40 40), Mass 100, enemyCollisionOptions)
  where enemyCollisionOptions = (CollisionFilter 2 (maskList [2]) (maskList [3]), CollisionType 2)

stepEnemies :: MonadIO m => SystemT World m ()
stepEnemies = stepArgoEnemies *> stepRunexEnemies

stepArgoEnemies :: MonadIO m => SystemT World m ()
stepArgoEnemies = do
  cmapM $ \(Enemy enemyType, Position pos) -> do
    case enemyType of
      Argo cooldown ->
        if cooldown < 1
          then spawnStraightBullet (Position pos) (Velocity $ V2 0 (-300)) False *> pure (Enemy (Argo 60))
          else pure (Enemy $ Argo $ pred cooldown)
      otherEnemy -> pure (Enemy otherEnemy)

stepRunexEnemies :: MonadIO m => SystemT World m ()
stepRunexEnemies = do
  cmapM $ \(Enemy enemyType, Position pos, Force originalForce) -> do
    case enemyType of
      (Runex speed endpoints) -> do
        pure (Force $ V2 (calcForce endpoints pos) 0)
      otherEnemy -> pure (Force originalForce)
  where
    calcForce (start, end) (V2 pos y) = calcForce2 (start, end) (V2 pos y) * 10000
    calcForce2 (start, end) (V2 pos _)
          | ((start + end) / 2) > pos = 1
          | otherwise = -1
          where distance = end - start



forEachEnemy :: P.Members [Embed IO, ApecsSystem World] r =>
  (EnemyType -> V2 Double -> Sem r ()) -> Sem r ()
forEachEnemy f = executeApecsSystem @World $
  cmapM_ $ \(Enemy enemyType, Position pos) -> lift (f (enemyType) pos)
