{-# LANGUAGE TypeFamilies #-}

module Player where

import Effect.Apecs
import Apecs.Physics
import Control.Monad.IO.Class
import Polysemy as P
import Polysemy.Input
import Sigma.Signal

import World
import Input

playerInitialize :: MonadIO m => SystemT World m Entity
playerInitialize = newEntity (Player, KinematicBody, Position (V2 0 0))

playerStep :: (Member (Embed IO) r, Member (Input GameInput) r) => SystemT World (Sem r) ()
playerStep = do
  GameInput u d l r s <- lift input
  let velocity = V2 (boolToNum r - boolToNum l) (boolToNum u - boolToNum d)
  cmap (\(Player, Position pos) -> Position (pos + velocity))
  where boolToNum True = 1
        boolToNum False = 0

getPlayerPosition :: P.Members [(ApecsSystem World), Embed IO] r => Signal (Sem r) (V2 Double)
getPlayerPosition = liftSem $ executeApecsSystem @World $ do
  pos <- cfold (\_ (Position pos) -> pos) (V2 0 0)
  pure pos

player :: P.Members [(ApecsSystem World), Embed IO, Input GameInput] r => Signal (Sem r) ()
player = withInitialization (executeApecsSystem $ playerInitialize >> playerStep) . const . liftSem $
  executeApecsSystem playerStep
