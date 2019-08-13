module Enemies where

import Effect.Apecs
import Apecs.Physics
import Control.Monad.IO.Class
import Polysemy as P
import Polysemy.Input
import Polysemy.Reader as R
import Sigma.Signal
import Control.Monad
import Debug.Trace

import Bullets
import World

test :: Monad m => ((m () -> m ()) -> IO ()) -> m () -> IO ()
test f x = f (const x)

enemies :: forall r. P.Members [ApecsSystem World, Embed IO] r => Signal (Sem r) ()
enemies = withInitialization initializeEnemies
              $ const $ liftSem $ executeApecsSystem @World $ stepArgoEnemies *> stepRunexEnemies

initializeEnemies :: P.Members [ApecsSystem World, Embed IO] r => Sem r ()
initializeEnemies = executeApecsSystem @World $ do

  collisionHandler <- createCollisionHandler
  newEntity collisionHandler

  argo <- newEntity (Enemy $ Argo 120, DynamicBody, Position (V2 220 300))
  newEntity (Shape argo (cRectangle $ V2 40 40), Mass 100, collisionFilter, CollisionType 2)

  runex <- newEntity (Enemy $ Runex 3 runexPathEndpoints, DynamicBody, Position (V2 100 400))
  newEntity (Shape runex (cRectangle $ V2 40 40), Mass 100, collisionFilter, CollisionType 2)


  pure ()
  where collisionFilter = CollisionFilter 2 (maskList [2]) (maskList [3])
        createCollisionHandler = do
          begin <- createBeginHandler
          pure $ CollisionHandler (Between 2 3) (Just begin) Nothing Nothing Nothing
            where
              createBeginHandler = mkBeginCB $ \(Collision _ enemy bullet enemyShape bulletShape) -> do
                addPostStepCallback 0 $ do
                  destroy enemy (Proxy :: Proxy (Enemy, Body))
                  destroy enemyShape (Proxy :: Proxy Shape)
                  destroy bullet (Proxy :: Proxy (Bullet, Body))
                  destroy bulletShape (Proxy :: Proxy Shape)
                pure False
        runexPathEndpoints :: (Double, Double)
        runexPathEndpoints = (100, 380)

          --fmap (flip V2 0) $ cycle ([5,4.9..(-5)]++ [-5,-4.90..5])


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
