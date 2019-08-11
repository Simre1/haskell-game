{-# LANGUAGE TemplateHaskell #-}

module Input where

import Polysemy
import Polysemy.Input
import Sigma
import Data.Maybe
import Lens.Micro
import Lens.Micro.TH
import Aux.Polysemy.Input
import qualified SDL

import Effect.Input

data GameInput = GameInput
  { _giUp :: Bool
  , _giDown :: Bool
  , _giLeft :: Bool
  , _giRight :: Bool
  , _giShoot :: Bool
  }

makeLenses ''GameInput

feedGameInput :: Member (Input SDLInput) r => Signal (Sem (Input GameInput : r)) a -> Signal (Sem r) a
feedGameInput = runInputWithSignal $ GameInput
  <$> fmap isJust (getKeyState ((==SDL.KeycodeW) . SDL.keysymKeycode))
  <*> fmap isJust (getKeyState ((==SDL.KeycodeS) . SDL.keysymKeycode))
  <*> fmap isJust (getKeyState ((==SDL.KeycodeA) . SDL.keysymKeycode))
  <*> fmap isJust (getKeyState ((==SDL.KeycodeD) . SDL.keysymKeycode))
  <*> fmap isJust (getKeyState ((==SDL.KeycodeSpace) . SDL.keysymKeycode))
