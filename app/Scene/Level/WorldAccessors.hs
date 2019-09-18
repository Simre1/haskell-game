{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Scene.Level.WorldAccessors where

import Control.Monad.IO.Class (MonadIO)
import Linear.V2 (V2(..))

import ECS.Apecs (ApecsSystem, executeApecsSystem, lift, cmapM_, cfold, SystemT)
import ECS.Physics (Position(..))

import Scene.Level.World (World, Bullet(..), BulletType, Enemy(..), EnemyType, Player(..))

forEachBullet :: MonadIO m => (BulletType -> V2 Double -> SystemT World m ()) -> SystemT World m ()
forEachBullet f = cmapM_ $ \(Bullet bulletType, Position pos) -> f (bulletType) pos

forEachEnemy :: MonadIO m => (EnemyType -> V2 Double -> SystemT World m ()) -> SystemT World m ()
forEachEnemy f = cmapM_ $ \(Enemy enemyType, Position pos) -> f (enemyType) pos

forPlayer :: MonadIO m => (Player -> V2 Double -> SystemT World m ()) -> SystemT World m ()
forPlayer f = cmapM_ $ \(Player c, Position pos) -> f (Player c) pos

getPlayerPosition :: MonadIO m => SystemT World m (V2 Double)
getPlayerPosition = cfold (\_ (Player _, Position pos) -> pos) (V2 240 100)

isPlayerAlive :: MonadIO m => SystemT World m Bool
isPlayerAlive = cfold (\_ (Player _) -> True) False

areEnemiesAlive :: MonadIO m => SystemT World m Bool
areEnemiesAlive = cfold (\_ (Enemy _) -> True) False
