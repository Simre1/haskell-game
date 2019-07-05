{-# LANGUAGE UndecidableInstances #-}
module Effect.Graphics
  ( Graphics
  , runGraphics
  , render
  , RenderAction
  , module RenderA
  , aquireGraphicsResource
  , GraphicsResource
  , module ResourceA
  , modifyCamera
  , getCamera
  , moveCamera
  , inverseY
  , adjustToCameraPosition
  , adjustYPosition
  ) where

import Import

import qualified SDL
import Data.Sequence
import Shapes2D
import Sigma

import Data.IORef
import Data.Coerce
import Control.Effect.Reader
import Data.StateVar
import Data.Word

import Effect.Graphics.Internal
import Effect.Graphics.RenderActions as RenderA
import Effect.Graphics.ResourceActions as ResourceA
