module Main where


import SDL (quit, initializeAll, createWindow, defaultWindow, createRenderer, defaultRenderer, windowResizable)
import Data.Text (pack)
import Polysemy (runM)
import Lens.Micro ((.~))

import Sigma (signalMorph, withInitialization)
import Sigma.Reactimate (reactimate)
import Sigma.Framerate (limitFramerate)

import Shapes2D
import Input.SDL (runSDLEventInput)
import Effect.Graphics (runGraphics, cameraArea, modifyCamera)
import Effect.Physics (runPhysics)

import Lens.Micro (sets)

import Player (player)

main :: IO ()
main = do
  initializeAll
  w <- createWindow (pack "Space Invaders") (defaultWindow {windowResizable = True})
  r <- createRenderer w 0 defaultRenderer
  runM $ reactimate $ limitFramerate 60 . runSDLEventInput . runGraphics r . runPhysics 60 $
    withInitialization (modifyCamera $ cameraArea . placedShape . rectangleDimensions .~ V2 480 480) . const $
      player
  quit
