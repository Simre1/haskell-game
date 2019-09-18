{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scene.Level.Step.Scenario where

import Control.Arrow (second)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<$))
import qualified Data.Vector as V
import Linear.V2 (V2(..))
import Polysemy (Members, Embed, Sem, raise)
import Polysemy.State (get, put, State)
import System.Random (StdGen, getStdGen, randomR)

import ECS.Apecs (SystemT, cmapM_, executeApecsSystem, ApecsSystem)
import Sigma (Signal, liftAction, withInitialization, feedback, stepSignal)

import Scene.Level.Spawn.Enemy (spawnEnemy)
import Scene.Level.World (World, EnemyType(..), QuantifiedSpeed(..))
import Scene.Level.WorldAccessors (getPlayerPosition, areEnemiesAlive)
import Scene.Scenes (Scene(..))



data WaveSpawnStrategy = Moved Int | Instant deriving Show

data Waves = Waves (V.Vector Scenario) [Wave]

data Scenario = Scenario [(EnemyType, V2 Double)]

data Wave = Wave Int WaveSpawnStrategy

level1Waves :: Waves
level1Waves = Waves (V.fromList $ sc1:sc2:sc3:sc4:sc5:sc6:[]) [Wave 0 Instant, Wave 1 (Moved 480), Wave 2 (Moved 480), Wave 3 (Moved 480), Wave 4 (Moved 480), Wave 5 (Moved 480)]

mkScenario :: [(EnemyType, V2 Double)] -> Scenario
mkScenario enemies = Scenario enemies

sc1 = mkScenario [(Argo 120 Fast, V2 240 0)]
sc2 = mkScenario [(Argo 120 Fast, V2 380 0), (Argo 120 Fast, V2 100 0)]
sc3 = mkScenario [(Argo 120 Fast, V2 150 0), (Argo 120 Fast, V2 330 0)]
sc4 = mkScenario [(Argo 120 Fast, V2 150 0), (Argo 120 Fast, V2 330 0), (Argo 120 Fast, V2 240 100)]
sc5 = mkScenario [(Argo 120 Fast, V2 150 0), (Argo 120 Fast, V2 330 0), (Argo 120 Fast, V2 240 0)]
sc6 = mkScenario [(Argo 120 Fast, V2 380 0), (Argo 120 Fast, V2 100 0), (Argo 120 Fast, V2 380 150), (Argo 120 Fast, V2 100 150)]


scenarioSignal :: forall r. Members [Embed IO, ApecsSystem World] r => (Waves, Scene) -> Signal (Sem r) (Maybe Scene)
scenarioSignal (Waves scenarios waves, sceneAfterCompletion) = feedback (pure (-2) :: Signal (Sem r) Int) . feedback (waves). liftAction $ go
  where
    go :: Sem (State ([Wave]):State (Signal (Sem r) Int):r) (Maybe Scene)
    go = do
      waves <- get
      (shouldSpawn, nextSpawnSignal) <- get >>= raise . raise . stepSignal
      let chooseAction x = case x of
            (-1) -> put nextSpawnSignal *> pure Nothing
            (-2) -> do
              if null waves
                then put @(Signal (Sem r) Int) (pure (-3))
                else do
                  let (Wave currentId spawnStrategy:nextWaves) = waves
                  put (makeSpawnStrategySignal currentId spawnStrategy)
                  put nextWaves
              pure Nothing
            (-3) -> executeApecsSystem areEnemiesAlive >>= \enemiesAlive -> if enemiesAlive
                      then pure Nothing
                      else pure (Just sceneAfterCompletion)
            spawnId -> do
              raise $ raise $ spawn spawnId
              chooseAction (-2)
      chooseAction shouldSpawn
      where
        spawn :: Int -> Sem r ()
        spawn spawnId = do
              let (Scenario enemies) = scenarios V.! spawnId
              (V2 _ playerY) <- executeApecsSystem getPlayerPosition
              executeApecsSystem $
                mapM_ (uncurry spawnEnemy) $ second (V2 0 playerY+V2 0 720+) <$> enemies
        makeSpawnStrategySignal :: Int -> WaveSpawnStrategy -> Signal (Sem r) Int
        makeSpawnStrategySignal spawnId strategy = case strategy of
          Instant -> pure spawnId
          (Moved amount) -> withInitialization (executeApecsSystem getPlayerPosition) $ \(V2 _ lastY) -> liftAction . executeApecsSystem $ do
            (V2 _ newY) <- getPlayerPosition
            if (floor newY) >= (floor lastY) + amount
              then pure spawnId
              else pure (-1)
