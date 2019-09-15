module Level.Spawn.Bullet where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Linear.V2 (V2(..))


import ECS.Physics (Body(..), Shape(..), Mass(..), Sensor (..), CollisionFilter(..), Velocity, Position, CollisionType(..), maskList, cRectangle)
import ECS.Apecs (SystemT, newEntity)

import Level.World (Bullet(..), BulletType(..), World)


spawnStraightBullet :: MonadIO m => Position -> Velocity -> Bool -> SystemT World m ()
spawnStraightBullet position velocity friendlyHostile = void $ do
  bullet <- newEntity (Bullet Straight, DynamicBody, position, velocity)
  bulletShape <- newEntity (Shape bullet (cRectangle (V2 12 12)), Mass 5, Sensor True, collisionConfig)
  pure ()
  where collisionConfig = case friendlyHostile of
          True -> (CollisionFilter 3 (maskList [3]) (maskList [2,4]), CollisionType 3)
          False -> (CollisionFilter 4 (maskList [4]) (maskList [1,3]), CollisionType 4)
