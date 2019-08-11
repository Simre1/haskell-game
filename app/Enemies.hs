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
              $ const $ liftSem $ stepArgoEnemies

initializeEnemies :: P.Members [ApecsSystem World, Embed IO] r => Sem r ()
initializeEnemies = executeApecsSystem @World $ do
  collisionHandler <- createCollisionHandler
  e <- newEntity (Enemy $ Argo 120, DynamicBody, Position (V2 220 400))
  newEntity (Shape e (cRectangle $ V2 40 40), Mass 100, collisionFilter, CollisionType 2)
  newEntity collisionHandler
  pure ()
  where collisionFilter = CollisionFilter 2 (maskList [2]) (maskList [3])
        createCollisionHandler = do
          begin <- createBeginHandler
          pure $ CollisionHandler (Between 2 3) (Just begin) Nothing Nothing Nothing
            where
              createBeginHandler = mkBeginCB $ \(Collision _ enemy _ shape _) -> do
                addPostStepCallback 0 $ do
                  destroy enemy (Proxy :: Proxy (Enemy, Body))
                  destroy shape (Proxy :: Proxy (Shape, CollisionHandler))
                pure False


stepArgoEnemies :: P.Members [ApecsSystem World, Embed IO] r => Sem r ()
stepArgoEnemies = executeApecsSystem @World $ do
  cmap $ \(Enemy (Argo cooldown)) -> Enemy (Argo $ pred cooldown)
  cmapM $ \(Enemy (Argo cooldown), Position pos) -> do
    if cooldown < 1
      then spawnStraightBullet (Position pos) (Velocity $ V2 0 (-300)) False *> pure (Enemy (Argo 60))
      else pure (Enemy (Argo cooldown))

forEachEnemy :: P.Members [Embed IO, ApecsSystem World] r =>
  (EnemyType -> V2 Double -> Sem r ()) -> Sem r ()
forEachEnemy f = executeApecsSystem @World $
  cmapM_ $ \(Enemy enemyType, Position pos) -> lift (f (enemyType) pos)
