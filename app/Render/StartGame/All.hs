{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}


module Render.StartGame.All where

import Polysemy (Member, Sem)

import Sigma (Signal)

import MyWindow (MyWindow)
import Render.PresentRender (presentRender)

renderStartGame :: Member MyWindow r => Signal (Sem r) ()
renderStartGame = presentRender $ pure ()
