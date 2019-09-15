module Level.Initialize.Player where

import Control.Monad.IO.Class (MonadIO)
import Linear.V2 (V2(..))

import ECS.Apecs (SystemT, newEntity)
import ECS.Physics (Body(..), Position(..), CollisionType(..), Shape(..), CollisionFilter(..), maskList, cRectangle)

import Level.World (World, Player(..))

initializePlayer :: MonadIO m => SystemT World m ()
initializePlayer = do
  playerBody <- newEntity (Player 0, KinematicBody, Position (V2 240 100))
  playerShape <- newEntity (Shape playerBody $ cRectangle $ V2 36 72, CollisionType 1, collisionFilter)
  pure ()
  where collisionFilter = CollisionFilter 1 (maskList [1]) (maskList [4])
