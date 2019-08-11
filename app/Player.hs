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
playerCooldown = 0.8

playerSpeed :: V2 Double -> V2 Double
playerSpeed = (*2) . \case
  (V2 1 1) -> V2 (sqrt 0.5) (sqrt 0.5)
  (V2 (-1) (-1)) -> negate <$> V2 (sqrt 0.5) (sqrt 0.5)
  (V2 x y) -> V2 x y

playerInitialize :: MonadIO m => SystemT World m Entity
playerInitialize = newEntity (Player 0, KinematicBody, Position (V2 0 0))

playerStep :: (Member (Embed IO) r, Member (Input GameInput) r) => SystemT World (Sem r) ()
playerStep = do
  GameInput u d l r s <- lift input
  let velocity = V2 (boolToNum r - boolToNum l) (boolToNum u - boolToNum d)
  cmap (\(Player c, Position pos) -> (Position (pos + playerSpeed velocity), Player (c - (6/100))))
  when s $ cmapM $ \(Player c, Position pos) ->
    if (c <= 0)
      then spawnStraightBullet (Position $ pos + V2 0 40) (Velocity $ V2 0 500) True *> pure (Player playerCooldown)
      else pure (Player c)
  where boolToNum True = 1
        boolToNum False = 0

getPlayerPosition :: P.Members [(ApecsSystem World), Embed IO] r => Signal (Sem r) (V2 Double)
getPlayerPosition = liftSem $ executeApecsSystem @World $ do
  pos <- cfold (\_ (Player _, Position pos) -> pos) (V2 0 0)
  pure pos

player :: P.Members [(ApecsSystem World), Embed IO, Input GameInput] r => Signal (Sem r) ()
player = withInitialization (executeApecsSystem $ playerInitialize >> playerStep) . const . liftSem $
  executeApecsSystem playerStep
