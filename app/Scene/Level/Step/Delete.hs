module Scene.Level.Step.Delete where

import Control.Monad.IO.Class (MonadIO)
import Linear.V2 (V2(..))

import ECS.Apecs (SystemT, cmapM, destroy, Not(..), Proxy(..))
import ECS.Physics (Position(..), ShapeList(..), Body, Shape)

import Scene.Level.World (World, Enemy, Player, Bullet)
import Scene.Level.WorldAccessors (getPlayerPosition)

deleteOutOfBounds :: (MonadIO m) => SystemT World m ()
deleteOutOfBounds = do
  playerPos <- getPlayerPosition
  cmapM $ \(Position pos, ShapeList [shape]) -> do
    if (outOfBounds playerPos pos)
      then destroy shape (Proxy :: Proxy Shape) *> (pure $ Right (Not :: Not (Player, Body, Enemy, Bullet)))
      else pure $ Left ()
  where outOfBounds (V2 pX pY) (V2 x y) = x < (-20) || x > 500 || y < pY - 1000 || y > pY + 1000
