module Main where

import SDL (quit, initializeAll, createWindow, defaultWindow, createRenderer, defaultRenderer, windowResizable)
import Data.Text (pack)
import Polysemy (runM)
import Lens.Micro ((.~))
import Polysemy.State
import Sigma (signalMorph, withInitialization)
import Sigma.Signal (liftSem)
import Sigma.Reactimate (reactimate)
import Sigma.Framerate (limitFramerate)

import Shapes2D
import Input.SDL (runSDLEventInput)
import Effect.Graphics (runGraphics, cameraArea, modifyCamera)
import Control.Monad.IO.Class

import Lens.Micro (sets)
import Linear.V2

import Effect.Apecs
import World
import Player
import Input
import Render

main :: IO ()
main = do
  initializeAll
  w <- createWindow (pack "Space Invaders") (defaultWindow {windowResizable = True})
  r <- createRenderer w 0 defaultRenderer
  runM $ reactimate $ limitFramerate 60 . runSDLEventInput . runGraphics r . runApecs (liftIO $ initWorld) . feedGameInput $
    withInitialization (modifyCamera $ cameraArea . placedShape . rectangleDimensions .~ V2 480 480) . const $
      player *> renderWorld
  quit
