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
  cmap $ \(Bullet t, Position pos) -> if (outOfBounds pos)
    then Nothing
    else Just (Bullet t)
  where outOfBounds (V2 x y) = x < (-20) || x > 500 || y < (-20) || y > 500

forEachBullet :: P.Members [Embed IO, ApecsSystem World] r =>
  (Bullet -> V2 Double -> Sem r ()) -> Sem r ()
forEachBullet f = executeApecsSystem @World $
  cmapM_ $ \(Bullet bulletType, Position pos) -> lift (f (Bullet bulletType) pos)

spawnStraightBullet :: MonadIO m => Position -> Velocity -> SystemT World m ()
spawnStraightBullet position velocity = void $ do
  bullet <- newEntity (Bullet Straight, DynamicBody, position, velocity)
  bulletShape <- newEntity (Shape bullet (cRectangle (V2 12 12)), Mass 10, Sensor True)
  pure ()
