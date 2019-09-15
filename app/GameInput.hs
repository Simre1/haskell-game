{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module GameInput where

import Graphics.GPipe.Context.GLFW (getKey, Key(Key'Up, Key'Left, Key'Right, Key'Space), KeyState(..))
import Polysemy (Member, Sem, reinterpret)
import Polysemy.Input (Input(..))
import Polysemy.Reader (Reader, ask, runReader)

import Sigma (Signal, signalMorph)
import Window.GPipe (makeWindowAction, executeWindowAction)

import MyWindow (MyWindow, MyWindowAction)

data GameInput = GameInput
  { giUp :: Bool
  , giLeft :: Bool
  , giRight :: Bool
  , giShoot :: Bool
  }


feedGameInput :: (Member MyWindow r) => Signal (Sem (Input GameInput : r)) a -> Signal (Sem r) a
feedGameInput = signalMorph $ \sem -> do
  i <- executeWindowAction input
  runReader i $ reinterpretInput sem
  where
    reinterpretInput :: Sem (Input GameInput:r) x -> Sem (Reader GameInput:r) x
    reinterpretInput = reinterpret (\Input -> ask)
    input :: MyWindowAction GameInput
    input = do
      makeWindowAction $ \window ->
        GameInput
          <$> (keyStateToBool <$> getKey window Key'Up)
          <*> (keyStateToBool <$> getKey window Key'Left)
          <*> (keyStateToBool <$> getKey window Key'Right)
          <*> (keyStateToBool <$> getKey window Key'Space)

      where keyStateToBool :: Maybe KeyState -> Bool
            keyStateToBool Nothing = False
            keyStateToBool (Just KeyState'Released) = False
            keyStateToBool _ = True
