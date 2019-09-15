{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Level.Step.Scenario where

import Control.Arrow (second)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Linear.V2 (V2(..))
import Polysemy (Members, Embed, Sem)
import Polysemy.State (get, put)
import System.Random (StdGen, getStdGen, randomR)

import ECS.Apecs (SystemT, cmapM_, executeApecsSystem, ApecsSystem)
import Sigma (Signal, liftAction, withInitialization, feedback)

import Level.Spawn.Enemy (spawnEnemy)
import Level.World (World, EnemyType(..))
import Level.WorldAccessors (getPlayerPosition)



data Scenario = Scenario Int [(EnemyType, V2 Double)]

data NextScenario = NextScenario Int Double StdGen

initializeScenarioState :: MonadIO m => m NextScenario
initializeScenarioState = NextScenario 0 0 <$> liftIO getStdGen

scenarios :: [Scenario]
scenarios = sc1:sc2:sc3:sc4:sc5:sc6:[]


mkScenario :: Int -> [(EnemyType, V2 Double)] -> Scenario
mkScenario difficulty enemies = Scenario difficulty enemies

sc1 = mkScenario 0 [(Argo 120, V2 240 0)]
sc2 = mkScenario 1 [(Argo 120, V2 380 0), (Argo 120, V2 100 0)]
sc3 = mkScenario 1 [(Argo 120, V2 150 0), (Argo 120, V2 330 0)]
sc4 = mkScenario 2 [(Argo 120, V2 150 0), (Argo 120, V2 330 0), (Argo 120, V2 240 100)]
sc5 = mkScenario 2 [(Argo 120, V2 150 0), (Argo 120, V2 330 0), (Argo 120, V2 240 0)]
sc6 = mkScenario 3 [(Argo 120, V2 380 0), (Argo 120, V2 100 0), (Argo 120, V2 380 150), (Argo 120, V2 100 150)]


scenarioSignal :: Members [Embed IO, ApecsSystem World] r => Signal (Sem r) ()
scenarioSignal = withInitialization initializeScenarioState $ \nextScenario -> feedback nextScenario . liftAction $ do
  (V2 _ playerY) <- executeApecsSystem getPlayerPosition
  NextScenario scenarioCount nextSpawnY randomGen <- get
  if playerY <= nextSpawnY
    then pure ()
    else do
      let (Scenario difficulty enemies, nextRandomGen) = selectScenario randomGen
      put $ calcNextScenario nextSpawnY scenarioCount difficulty nextRandomGen
      executeApecsSystem $
        mapM_ (uncurry spawnEnemy) $ second (V2 0 playerY+V2 0 720+) <$> enemies
  where
    selectScenario gen =
      let (rNumber, newGen) = randomR (0, pred $ length scenarios) gen
      in (scenarios !! rNumber,newGen)
    calcNextScenario currentSpawnY scenarioCount difficulty = NextScenario (succ scenarioCount) $ currentSpawnY + (adaptToScenarioCount $ 400 + (fromIntegral difficulty) * 60)
      where adaptToScenarioCount x = (x * x) / (x + (fromIntegral scenarioCount) * 8)
