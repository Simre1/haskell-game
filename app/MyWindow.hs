module MyWindow where

import Graphics.GPipe.Context.GLFW (Handle)

import Window.GPipe (WindowIO, WindowAction, RGBAFloat)


type MyWindow = WindowIO Handle RGBAFloat ()

type MyWindowAction a = WindowAction Handle RGBAFloat () a
