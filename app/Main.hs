module Main where

import Graphics.GPipe.Format (WindowFormat(..), Format(..))
import Graphics.GPipe.Context.GLFW (defaultHandleConfig, defaultWindowConfig)

import Sigma (limitFramerate)
import Window.GPipe (runGPipe, RGBAFloat)

import Scene.SceneManager (sceneManager)

import GameInput

main :: IO ()
main = runGPipe defaultHandleConfig (WindowFormatColor $ RGBA12) (defaultWindowConfig "Test")
         . feedGameInput . limitFramerate 60 $ sceneManager
