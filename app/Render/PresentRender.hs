{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Render.PresentRender where

import Polysemy (Sem, Member)

import Sigma (Signal, liftAction)
import Window.GPipe (executeWindowAction, clearWindowColor, swapWindowBuffers, makeWindowAction, render)

import MyWindow (MyWindow, MyWindowAction)

presentRender :: Member MyWindow r => Signal (Sem r) () -> Signal (Sem r) ()
presentRender sig = (liftAction $ executeWindowAction clearWindow) *> sig *> (liftAction $ executeWindowAction swapBuffers)
  where
    swapBuffers :: MyWindowAction ()
    swapBuffers = makeWindowAction $ \win -> swapWindowBuffers win

    clearWindow :: MyWindowAction ()
    clearWindow = makeWindowAction $ \win -> render $ clearWindowColor win 0.5
