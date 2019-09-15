{-# LANGUAGE GADTs #-} -- To pattern match on 'ContextFormat' constructors.
{-# LANGUAGE DeriveAnyClass #-} -- To derive 'Exception' w/o a standalone declaration.
{-# LANGUAGE FlexibleInstances #-} -- To derive 'Exception [WindowHint]'.
-- | Internal module for generating and assessing GLFW hint lists
module Graphics.GPipe.Context.GLFW.Format where

-- stdlib
import Control.Exception (Exception)
-- third party
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.GPipe as GPipe
import Graphics.UI.GLFW (WindowHint(..))

-- | IO Exception thrown when attempting to create a new window using GLFW
-- hints which GPipe manages.
data UnsafeWindowHintsException = UnsafeWindowHintsException [WindowHint]
    deriving
    ( Exception
    , Show
    )
instance Exception [WindowHint]

allowedHint :: WindowHint -> Bool
allowedHint (WindowHint'Visible _) = False
allowedHint (WindowHint'sRGBCapable _) = False
allowedHint (WindowHint'RedBits _) = False
allowedHint (WindowHint'GreenBits _) = False
allowedHint (WindowHint'BlueBits _) = False
allowedHint (WindowHint'AlphaBits _) = False
allowedHint (WindowHint'DepthBits _) = False
allowedHint (WindowHint'StencilBits _) = False
allowedHint (WindowHint'ContextVersionMajor _) = False
allowedHint (WindowHint'ContextVersionMinor _) = False
allowedHint (WindowHint'OpenGLForwardCompat _) = False
allowedHint (WindowHint'OpenGLProfile _) = False
allowedHint _ = True

unconditionalHints :: [GLFW.WindowHint]
unconditionalHints =
    [ GLFW.WindowHint'ContextVersionMajor 3
    , GLFW.WindowHint'ContextVersionMinor 3
    , GLFW.WindowHint'OpenGLForwardCompat True
    , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    ]

bitsToHints :: Maybe GPipe.WindowBits -> [GLFW.WindowHint]
bitsToHints Nothing = [GLFW.WindowHint'Visible False]
bitsToHints (Just ((red, green, blue, alpha, sRGB), depth, stencil)) =
    [ GLFW.WindowHint'sRGBCapable sRGB
    , GLFW.WindowHint'RedBits $ Just red
    , GLFW.WindowHint'GreenBits $ Just green
    , GLFW.WindowHint'BlueBits $ Just blue
    , GLFW.WindowHint'AlphaBits $ Just alpha
    , GLFW.WindowHint'DepthBits $ Just depth
    , GLFW.WindowHint'StencilBits $ Just stencil
    ]
