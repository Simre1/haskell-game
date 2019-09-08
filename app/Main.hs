{-# LANGUAGE TypeFamilies #-}

module Main where

import Sigma

import Graphics.GPipe.Context.GLFW
import Graphics.GPipe.Format (Format(RGBA12), WindowFormat(WindowFormatColor), RGBAFloat)

import Control.Monad.IO.Class
import Data.Text (Text, pack)
import Polysemy
import GPipe.Interface
import Polysemy.Reader

import Effect.Apecs
import Render
import Step
import Input
import Types

main :: IO ()
main = do
  runGPipe defaultHandleConfig (WindowFormatColor $ RGBA12) (defaultWindowConfig "Test") . limitFramerate 60 . runApecs (liftIO initWorld) . feedGameInput $
    withInitialization (initializeGameState *> initializeRenderData) $ \renderData ->
      step *> signalMorph (runReader renderData) renderWorld *> pure False
