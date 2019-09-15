{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Render.Level.All where

import Polysemy (Members, Embed, Sem)
import Linear.V2 (V2(..))

import ECS.Apecs (ApecsSystem, lift, executeApecsSystem)
import Window.GPipe (executeWindowAction, makeWindowAction, clearWindowColor, swapWindowBuffers, render)
import Sigma (Signal, liftAction, withInitialization)

import Level.World (World)
import Level.WorldAccessors (getPlayerPosition, forEachBullet, forEachEnemy)
import MyWindow (MyWindow, MyWindowAction)
import Render.Level.Background (makeBackgroundShader)
import Render.Level.Enemy (makeArgoShader)
import Render.Level.Player (makePlayerShader)
import Render.Level.Bullet (makeStraightBulletShader)


renderLevel :: Members [Embed IO, ApecsSystem World, MyWindow] r => Signal (Sem r) ()
renderLevel = withInitialization (executeWindowAction initializeLevelRenderData) $ \renderData -> liftAction $ do
  V2 x y <- executeApecsSystem getPlayerPosition
  executeWindowAction clearWindow
  executeWindowAction $ do
    backgroundShader renderData (realToFrac y)
    playerShader renderData (round <$> V2 x 100)
  executeApecsSystem $ do
    forEachBullet (\_ pos -> lift $ executeWindowAction $ straightBulletShader renderData (round <$> pos - V2 0 (y-100)))
    forEachEnemy (\_ pos -> lift $ executeWindowAction $ argoEnemyShader renderData (round <$> pos - V2 0 (y - 100)))
  executeWindowAction swapBuffers

swapBuffers :: MyWindowAction ()
swapBuffers = makeWindowAction $ \win -> swapWindowBuffers win

clearWindow :: MyWindowAction ()
clearWindow = makeWindowAction $ \win -> render $ clearWindowColor win 0.5

initializeLevelRenderData :: MyWindowAction LevelRenderData
initializeLevelRenderData = LevelRenderData <$> makePlayerShader <*> makeBackgroundShader <*> makeStraightBulletShader <*> makeArgoShader

data LevelRenderData = LevelRenderData
  { playerShader :: V2 Int -> MyWindowAction ()
  , backgroundShader :: Float -> MyWindowAction ()
  , straightBulletShader :: V2 Int -> MyWindowAction ()
  , argoEnemyShader :: V2 Int -> MyWindowAction ()
  }
