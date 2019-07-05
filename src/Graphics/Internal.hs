{-# LANGUAGE TemplateHaskell #-}

module Graphics.Internal (module Graphics.TextureBuilder, Graphics, makeTexture, runGraphics, renderTexture, RenderTexture, Camera, cameraPosition, render, cameraUse, cameraModify) where

import qualified SDL
import Data.Sequence hiding (empty)
import Data.IORef
import Data.Coerce
import Data.StateVar
import Data.Word
import Control.Concurrent
import Control.Concurrent.MVar
import Lens.Micro
import Lens.Micro.Extras
import Lens.Micro.TH
import Graphics.TextureBuilder
import Data.ByteString.Builder as B
import Control.Monad.IO.Class
import Polysemy
import Control.Monad

import Shapes2D
import Sigma


data Camera = Camera {_cameraPosition :: Placed Rectangle Int}

makeLenses ''Camera

data GraphicsData = GraphicsData
  { dataRenderObjects :: IORef (Seq RenderTexture)
  , dataRenderer :: SDL.Renderer
  , dataCamera :: IORef Camera
  }

data RenderTexture = RenderTexture
  { zIndex :: Int
  , ctTexture :: SDL.Texture
  , ctSourceRect :: Maybe (Placed Rectangle Int)
  , ctDestinationRectangle :: Maybe (Placed Rectangle Int)
  }

renderTexture :: Int -> Texture a -> Maybe (Placed Rectangle Int) -> Maybe (Placed Rectangle Int) -> RenderTexture
renderTexture i (Texture _ sdlTexture) = RenderTexture i sdlTexture

initGraphicsData :: MonadIO m => Camera -> SDL.Renderer -> m GraphicsData
initGraphicsData camera renderer = liftIO $ GraphicsData <$> newIORef mempty
                                                <*> pure renderer
                                                <*> newIORef camera


data Graphics (m :: * -> *) k where
  Render :: RenderTexture -> Graphics m ()
  MakeTexture :: TextureBuilder (Texture a) -> Graphics m (Texture a)
  CameraUse :: SimpleGetter Camera a -> Graphics m a
  CameraModify :: ASetter Camera Camera a b -> (a -> b) -> Graphics m ()


makeSem ''Graphics

runGraphicsE :: Member (Lift IO) r => (IO () -> IO ()) -> GraphicsData -> Sem (Graphics : r) a -> Sem r a
runGraphicsE addTextureAction gd = interpret $ \case
  (Render renderTexture) -> liftIO $ modifyIORef' (dataRenderObjects gd) (:|> renderTexture)
  (CameraUse getter) -> liftIO . fmap (view getter) $ readIORef (dataCamera gd)
  (CameraModify setter f) -> liftIO $ modifyIORef (dataCamera gd) (over setter f)
  (MakeTexture textureBuilder) -> liftIO $ do
    preferredFormat <- findFormat
    (texture, textureAction) <- runTextureBuilder (dataRenderer gd) preferredFormat (rgbaToPreferredFormat preferredFormat) textureBuilder
    addTextureAction textureAction
    pure texture
    where findFormat = pure SDL.ARGB8888 -- TODO: Actually find out the best format!
          rgbaToPreferredFormat SDL.ARGB8888 (r,g,b,a) = B.word8 b <> B.word8 g <> B.word8 r <> B.word8 a

runGraphics :: (Member (Lift IO) r) => SDL.Renderer -> Signal (Sem (Graphics:r)) a b -> Signal (Sem r) a b
runGraphics renderer signal = Signal $ \a -> do
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  graphicsData <- initGraphicsData defCamera renderer
  delayedTextureActions <- liftIO $ newIORef mempty
  renderObjectsMVar <- liftIO newEmptyMVar

  tId <- liftIO . forkIO $ forever $ do
    (delayedAction, textures, camera) <- takeMVar renderObjectsMVar
    delayedAction
    renderObjects renderer textures camera
  let makeSig sig = Signal $ \a -> do
        liftIO $ writeIORef (dataRenderObjects graphicsData) mempty
        (b, cont) <- stepSignal sig a
        textures <- liftIO $ readIORef (dataRenderObjects graphicsData)
        camera <- liftIO $ readIORef (dataCamera graphicsData)
        delayedAction <- liftIO $ atomicModifyIORef' delayedTextureActions $ \actions -> (mempty, actions)
        liftIO $ tryTakeMVar renderObjectsMVar >> putMVar renderObjectsMVar (delayedAction,textures,camera)
        pure (b,makeSig cont)
  stepSignal (signalSimpleMorph (runGraphicsE (\newAction -> modifyIORef delayedTextureActions (>>newAction)) graphicsData) $ makeSig signal) a
    where
      defCamera = Camera $ Placed (V2 0 0) $ newRectangle (V2 800 600)
      renderObjects renderer textures camera = do
        SDL.rendererRenderTarget renderer $= Nothing
        SDL.rendererDrawColor renderer $= SDL.V4 maxBound maxBound maxBound maxBound
        SDL.clear renderer
        liftIO $ mapM_ runRender (sortBy (\r1 r2 -> zIndex r1 `compare` zIndex r2) textures)
        SDL.present renderer
        pure ()
          where runRender (RenderTexture _ texture srcRect destRect) =
                  let newDestRect = destRect <&> over placedPosition (adjustPointForRender camera)
                  in do
                    SDL.copyEx renderer texture (transformRectangle<$>srcRect) (transformRectangle<$>newDestRect) 0 Nothing (V2 False True)


adjustPointForRender :: Camera -> V2 Int -> V2 Int
adjustPointForRender (Camera (Placed (V2 cX cY) (Rectangle (V2 _ rY)))) (V2 x y) =
  V2 (x - cX) (rY - y + cY)
