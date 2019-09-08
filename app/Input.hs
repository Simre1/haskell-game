{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Input where

import Polysemy
import Polysemy.Input
import Sigma
import Data.Maybe
import Control.Lens
import Polysemy.Reader
import GPipe.Interface
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Types

data GameInput = GameInput
  { _giUp :: Bool
  , _giDown :: Bool
  , _giLeft :: Bool
  , _giRight :: Bool
  , _giShoot :: Bool
  }

makeLenses ''GameInput


feedGameInput :: (Member MyWindowIO r) => Signal (Sem (Input GameInput : r)) a -> Signal (Sem r) a
feedGameInput = signalMorph $ \sem -> do
  i :: GameInput <- executeWindowAction input
  runReader i $ reinterpretInput sem
  where
    reinterpretInput :: Sem (Input GameInput:r) x -> Sem (Reader GameInput:r) x
    reinterpretInput = reinterpret (\Input -> ask)
    input :: MyWindowAction GameInput
    input = do
      makeWindowAction $ \window ->
        GameInput
          <$> (keyStateToBool <$> GLFW.getKey window GLFW.Key'Up)
          <*> (keyStateToBool <$> GLFW.getKey window GLFW.Key'Down)
          <*> (keyStateToBool <$> GLFW.getKey window GLFW.Key'Left)
          <*> (keyStateToBool <$> GLFW.getKey window GLFW.Key'Right)
          <*> (keyStateToBool <$> GLFW.getKey window GLFW.Key'Space)

      where keyStateToBool :: Maybe GLFW.KeyState -> Bool
            keyStateToBool Nothing = False
            keyStateToBool (Just GLFW.KeyState'Released) = False
            keyStateToBool _ = True
