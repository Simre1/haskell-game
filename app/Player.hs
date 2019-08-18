{-# LANGUAGE TypeFamilies #-}

module Player where

import Effect.Apecs
import Apecs.Physics
import Control.Monad.IO.Class
import Polysemy as P
import Polysemy.Input
import Sigma.Signal
import Control.Monad
import Debug.Trace

import World
import Input
import Bullets

playerCooldown :: Double
playerCooldown = 1.5

playerSpeed :: V2 Double -> V2 Double
playerSpeed = (*80) . \case
  (V2 1 1) -> V2 (sqrt 0.5) (sqrt 0.5)
  (V2 (-1) (-1)) -> negate <$> V2 (sqrt 0.5) (sqrt 0.5)
  (V2 x y) -> V2 x y

initializePlayer :: MonadIO m => SystemT World m ()
initializePlayer = do
  playerBody <- newEntity (Player 0, KinematicBody, Position (V2 240 100))
  playerShape <- newEntity (Shape playerBody $ cRectangle $ V2 36 72, CollisionType 1, collisionFilter)
  pure ()
  where collisionFilter = CollisionFilter 1 (maskList [1]) (maskList [4])

stepPlayer :: (Member (Embed IO) r, Member (Input GameInput) r) => SystemT World (Sem r) ()
stepPlayer = do
  GameInput u d l r s <- lift input
  let velocity = V2 (boolToNum r - boolToNum l) (boolToNum u - boolToNum d)
  cmap $ \(Player c, Velocity v) -> (Velocity (playerSpeed velocity), Player (c - (6/100)))
  cmap $ \(Player c, Position (V2 x y)) ->
         if x <= 18
           then Position (V2 18 y)
           else if x >= 462
             then Position (V2 462 y)
             else Position (V2 x y)

  when s $ cmapM $ \(Player c, Position pos) -> do
    if (c <= 0)
      then spawnStraightBullet (Position $ pos + V2 0 40) (Velocity $ V2 0 500) True *> pure (Player playerCooldown)
      else pure (Player c)
  where boolToNum True = 1
        boolToNum False = 0

getPlayerPositionSignal :: P.Members [(ApecsSystem World), Embed IO] r => Signal (Sem r) (V2 Double)
getPlayerPositionSignal = liftAction $ executeApecsSystem @World $ do
  pos <- cfold (\_ (Player _, Position pos) -> pos) (V2 240 100)
  pure pos

getPlayerPosition :: MonadIO m => SystemT World m (V2 Double)
getPlayerPosition = do
  cfold (\_ (Player _, Position pos) -> pos) (V2 240 100)
