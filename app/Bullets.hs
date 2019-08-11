module Bullets where

import Effect.Apecs
import Apecs.Physics
import Control.Monad.IO.Class
import Polysemy as P
import Polysemy.Input
import Sigma.Signal
import Control.Monad
import Debug.Trace
import World

bullets :: P.Members [Embed IO, ApecsSystem World] r => Signal (Sem r) ()
bullets = liftSem $ executeApecsSystem @World $ do
  cmapM $ \(Bullet t, Position pos, ShapeList [shape], body :: Body) -> if (outOfBounds pos)
    then destroy shape (Proxy @(Shape)) *> pure Nothing
    else pure $ Just (Bullet t, body)
  where outOfBounds (V2 x y) = x < (-20) || x > 500 || y < (-20) || y > 500

forEachBullet :: P.Members [Embed IO, ApecsSystem World] r =>
  (BulletType -> V2 Double -> Sem r ()) -> Sem r ()
forEachBullet f = executeApecsSystem @World $
  cmapM_ $ \(Bullet bulletType, Position pos) -> lift (f (bulletType) pos)

spawnStraightBullet :: MonadIO m => Position -> Velocity -> Bool -> SystemT World m ()
spawnStraightBullet position velocity friendlyHostile = void $ do
  bullet <- newEntity (Bullet Straight, DynamicBody, position, velocity)
  bulletShape <- newEntity (Shape bullet (cRectangle (V2 12 12)), Mass 5, Sensor True, collisionConfig)
  pure ()
  where collisionConfig = case friendlyHostile of
          True -> (CollisionFilter 3 (maskList [3]) (maskList [2]), CollisionType 3)
          False -> (CollisionFilter 4 (maskList [4]) (maskList [1]), CollisionType 4)
