{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Level.Step.Player where

import Control.Monad (when)
import Linear.V2 (V2(..))
import Polysemy (Member, Embed, Sem)
import Polysemy.Input (Input, input)

import ECS.Apecs (SystemT, lift, cmap, cmapM)
import ECS.Physics (Position(..), Velocity(..))

import GameInput (GameInput(..))
import Level.World (World, Player(..))
import Level.Spawn.Bullet (spawnStraightBullet)

playerCooldown :: Double
playerCooldown = 1.5

playerSpeed :: V2 Double -> V2 Double
playerSpeed = (*120) . \case
  (V2 1 1) -> V2 (sqrt 0.5) (sqrt 0.5)
  (V2 (-1) (-1)) -> negate <$> V2 (sqrt 0.5) (sqrt 0.5)
  (V2 x y) -> V2 x y


stepPlayer :: (Member (Embed IO) r, Member (Input GameInput) r) => SystemT World (Sem r) ()
stepPlayer = do
  GameInput u l r s <- lift input
  let velocity = V2 (boolToNum r - boolToNum l) (0.5 + boolToNum u)
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
