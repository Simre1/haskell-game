module SDLTesting where

import Import
import qualified SDL
import Control.Concurrent
import Foreign.Storable
import Foreign.Ptr
import Data.Word
import Data.StateVar
import Linear.V4

sdlTest :: IO ()
sdlTest = do
  SDL.initializeAll
  window <- SDL.createWindow "Hello" SDL.defaultWindow
  renderer <- SDL.createRenderer window 0 SDL.defaultRenderer
  texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget (V2 800 600)
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  SDL.rendererRenderTarget renderer $= Just texture
  SDL.fillRect renderer Nothing
  SDL.rendererRenderTarget renderer $= Nothing

  let
    loop 0 = pure ()
    loop n = SDL.copy renderer texture Nothing Nothing >> threadDelay 1000 >> loop (n - 1) >> SDL.present renderer
  loop 30000
  SDL.quit
  pure ()

sdlTest2 :: IO ()
sdlTest2 = do
  SDL.initializeAll
  window <- SDL.createWindow "Hello" SDL.defaultWindow
  renderer <- SDL.createRenderer window 0 SDL.defaultRenderer
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  let
    loop 0 = pure ()
    loop n = SDL.fillRect renderer Nothing >> threadDelay 1000 >> loop (n - 1) >> SDL.present renderer
  loop 30000
  SDL.quit
  pure ()
