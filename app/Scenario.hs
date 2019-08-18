module Scenario where

import Effect.Apecs
import Apecs.Physics
import Control.Monad.IO.Class
import Polysemy as P
import Polysemy.Input
import Sigma.Signal
import Control.Monad
import Debug.Trace
import World
import Control.Arrow (second)
import System.Random
import Enemies
import Player


data Scenario = Scenario Int [(EnemyType, V2 Double)]

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


initializeScenarios :: MonadIO m => SystemT World m ()
initializeScenarios = void $ liftIO getStdGen >>= \gen -> newEntity (ScenarioTimer 0 0 gen)

stepScenarios :: MonadIO m => SystemT World m ()
stepScenarios = do
  (V2 _ playerY) <- getPlayerPosition
  cmapM $ \(ScenarioTimer scenarioCount timer randomGen) -> if timer < 1
      then do
        let (Scenario difficulty enemies, newGen) = selectScenario randomGen
        spawn $ second (V2 0 playerY+V2 0 720+) <$> enemies
        pure $ calcNewScenarioTimer scenarioCount difficulty newGen
      else pure $ ScenarioTimer scenarioCount (pred timer) randomGen
  where
    selectScenario gen =
      let (rNumber, newGen) = randomR (0, pred $ length scenarios) gen
      in (scenarios !! rNumber,newGen)
    spawn enemies = mapM_ (uncurry spawnEnemy) enemies
    calcNewScenarioTimer scenarioCount difficulty = ScenarioTimer (succ scenarioCount) $ adaptToScenarioCount $ 300 + difficulty * 60
      where adaptToScenarioCount x = ((100 * x * x) `quot` (x + scenarioCount)) `quot` 100
