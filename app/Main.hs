module Main where


import SDL (quit, initializeAll, createWindow, defaultWindow, createRenderer, defaultRenderer)
import Data.Text (pack)
import Polysemy (runM)


import Sigma (signalSimpleMorph)
import Sigma.Reactimate (reactimate)
import Sigma.Framerate (limitFramerate)

import Input.SDL (runSDLEventInput)
import Effect.Graphics (runGraphics)
import Effect.Physics (runPhysics)

import Player (player)

main :: IO ()
main = do
  initializeAll
  w <- createWindow (pack "Space Invaders") defaultWindow
  r <- createRenderer w 0 defaultRenderer
  reactimate $ limitFramerate 60 . signalSimpleMorph runM . runSDLEventInput . runGraphics r . runPhysics 60 $
    player
  quit
