module Main where

import SDL (quit, initializeAll, createWindow, defaultWindow, createRenderer, defaultRenderer, windowResizable)
import SDL.Video.OpenGL (defaultOpenGL)
import Data.Text (pack)
import Polysemy (runM)
import Lens.Micro ((.~))
import Polysemy.State
import Sigma (signalMorph, withInitialization)
import Sigma.Signal (liftAction)
import Sigma.Reactimate (reactimate)
import Sigma.Framerate (limitFramerate)

import Shapes2D
import Input.SDL (runSDLEventInput)
import Effect.Graphics (runGraphics, cameraArea, modifyCamera)
import Control.Monad.IO.Class
import Apecs.Physics hiding (Members)

import Lens.Micro (sets)
import Linear.V2
import Polysemy as P
import Polysemy.Input
import Sigma.Signal

import Effect.Apecs
import World
import Player
import Input
import Render
import Bullets
import Enemies
import Scenario
import Collision
import Step

main :: IO ()
main = do
  initializeAll
  w <- createWindow (pack "Space Invaders") (defaultWindow {windowResizable = True})
  r <- createRenderer w 0 defaultRenderer
  runM $ reactimate $ limitFramerate 60 . runSDLEventInput . runGraphics r . feedGameInput $ handleGameLoop gameLoop
  quit
  where gameLoop =
            withInitialization ((modifyCamera $ cameraArea . placedShape . rectangleDimensions .~ V2 480 480) *> initialize) . const $
              liftAction step *> renderWorld  *> isAlive
        handleGameLoop sig =  Signal $ (\w -> ((), handleGameLoop' w sig)) <$> liftIO initWorld
        handleGameLoop' world sig = Signal $ do
          (alive, nextSig) <- runApecsSystem world $ stepSignal sig
          if alive then pure ((), handleGameLoop' world nextSig) else (\w -> ((), handleGameLoop' w gameLoop)) <$> liftIO initWorld



initialize :: Members [Embed IO, ApecsSystem World] r => Sem r ()
initialize = executeApecsSystem @World $ do
  initializePlayer
  initializeCollisionHandlers
  initializeScenarios

isAlive :: Members [Embed IO, ApecsSystem World] r => Signal (Sem r) Bool
isAlive = liftAction $ executeApecsSystem @World $ cfold findPlayer False
  where
    findPlayer :: Bool -> Player -> Bool
    findPlayer found pl = True
