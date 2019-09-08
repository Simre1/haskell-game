module Step where


import Effect.Apecs
import Apecs.Core hiding (Members)
import Apecs.Physics hiding (Members)
import Control.Monad.IO.Class
import Polysemy as P
import Polysemy.Input
import Sigma.Signal
import Control.Monad
import Debug.Trace

import Types
import Input
import Player
import Enemies
import Bullets
import Scenario
import Collision

initializeGameState :: Members [Embed IO, ApecsSystem World, Input GameInput] r => Sem r ()
initializeGameState = executeApecsSystem @World $ do
  initializePlayer
  initializeScenarios
  initializeCollisionHandlers

step :: Members [Embed IO, ApecsSystem World, Input GameInput] r => Signal (Sem r) ()
step = liftAction $ executeApecsSystem @World $ do
  stepPlayer
  stepEnemies
  stepScenarios
  stepPhysics (1/60)
  deleteOutOfBounds


deleteOutOfBounds :: (MonadIO m) => SystemT World m ()
deleteOutOfBounds = do
  playerPos <- getPlayerPosition
  cmapM $ \(Position pos, ShapeList [shape]) -> do
    if (outOfBounds playerPos pos)
      then destroy shape (Proxy :: Proxy Shape) *> (pure $ Right (Not :: Not (Player, Body, Enemy, Bullet)))
      else pure $ Left ()
  where outOfBounds (V2 pX pY) (V2 x y) = x < (-20) || x > 500 || y < pY - 1000 || y > pY + 1000
