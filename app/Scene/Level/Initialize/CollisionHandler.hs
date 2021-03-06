module Scene.Level.Initialize.CollisionHandler where

import Control.Monad.IO.Class (MonadIO)

import ECS.Apecs (SystemT, newEntity, Proxy(..), destroy)
import ECS.Physics (mkBeginCB, CollisionHandler(..), Collision(..), Shape, Body, CollisionSource(Between), addPostStepCallback)

import Scene.Level.World (World, Bullet, Player, Enemy)


initializeCollisionHandlers :: MonadIO m => SystemT World m ()
initializeCollisionHandlers = do
  createPlayerCollisionHandler >>= newEntity
  createEnemiesCollisionHandler >>= newEntity
  createBulletCollisionHandler >>= newEntity
  pure ()

createEnemiesCollisionHandler :: MonadIO m => SystemT World m CollisionHandler
createEnemiesCollisionHandler = do
  begin <- mkBeginCB $ \(Collision _ enemy bullet enemyShape bulletShape) -> do
    addPostStepCallback 1 $ do
      destroy enemy (Proxy :: Proxy (Enemy, Body))
      destroy enemyShape (Proxy :: Proxy Shape)
      destroy bullet (Proxy :: Proxy (Bullet, Body))
      destroy bulletShape (Proxy :: Proxy Shape)
    pure False
  pure $ CollisionHandler (Between 2 3) (Just begin) Nothing Nothing Nothing


createPlayerCollisionHandler :: MonadIO m => SystemT World m CollisionHandler
createPlayerCollisionHandler = do
  beginCB <- mkBeginCB $ \(Collision _ player bullet playerShape bulletShape) -> do
    addPostStepCallback 2 $ do
      destroy player (Proxy :: Proxy (Player, Body))
      destroy playerShape (Proxy :: Proxy Shape)
      destroy bullet (Proxy :: Proxy (Bullet, Body))
      destroy bulletShape (Proxy :: Proxy Shape)
    pure False
  pure $ CollisionHandler (Between 1 4) (Just beginCB) Nothing Nothing Nothing

createBulletCollisionHandler :: MonadIO m => SystemT World m CollisionHandler
createBulletCollisionHandler = do
  beginCB <- mkBeginCB $ \(Collision _ bullet1 bullet2 bullet1Shape bullet2Shape) -> do
    addPostStepCallback 3 $ do
      destroy bullet1 (Proxy :: Proxy (Bullet, Body))
      destroy bullet1Shape (Proxy :: Proxy Shape)
      destroy bullet2 (Proxy :: Proxy (Bullet, Body))
      destroy bullet2Shape (Proxy :: Proxy Shape)
    pure False
  pure $ CollisionHandler (Between 3 4) (Just beginCB) Nothing Nothing Nothing
