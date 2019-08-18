{-# LANGUAGE TemplateHaskell #-}

module Graphics.Internal
  (module Graphics.TextureBuilder
  , Graphics
  , makeTexture
  , destroyTexture
  , runGraphics
  , makeRenderInstruction
  , defaultRenderInstruction
  , RenderInstruction
  , Camera
  , cameraArea
  , render
  , getCamera
  , modifyCamera
  , riZIndex
  , riTexture
  , riTextureArea
  , riScreenArea
  , riRotation
  , riRotationPoint
  , riFlip
  ) where

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
import Foreign.C.Types (CDouble(CDouble))

import Shapes2D
import Sigma
import Graphics.Image


data Camera = Camera {_cameraArea :: Placed Rectangle Int}

makeLenses ''Camera

data GraphicsData = GraphicsData
  { dataRenderObjects :: IORef (Seq RenderInstruction)
  , dataRenderer :: SDL.Renderer
  , dataCamera :: IORef Camera
  }

data RenderInstruction = RenderInstruction
  { _riZIndex :: Int
  , _riTexture :: Texture Any
  , _riTextureArea :: Maybe (Placed Rectangle Int)
  , _riScreenArea :: Maybe (Placed Rectangle Int)
  , _riRotation :: Double
  , _riRotationPoint :: Maybe (V2 Int)
  , _riFlip :: V2 Bool
  }

makeLenses ''RenderInstruction

makeRenderInstruction :: Int -> Texture Any -> Maybe (Placed Rectangle Int) -> Maybe (Placed Rectangle Int) -> Double -> Maybe (V2 Int) -> V2 Bool -> RenderInstruction
makeRenderInstruction = RenderInstruction

defaultRenderInstruction :: ToAnyTexture a => Texture a -> RenderInstruction
defaultRenderInstruction texture = makeRenderInstruction 0 (toAnyTexture texture) Nothing Nothing 0 Nothing (V2 False False)

initGraphicsData :: MonadIO m => Camera -> SDL.Renderer -> m GraphicsData
initGraphicsData camera renderer = liftIO $ GraphicsData <$> newIORef mempty
                                                <*> pure renderer
                                                <*> newIORef camera


data Graphics (m :: * -> *) k where
  Render :: RenderInstruction -> Graphics m ()
  MakeTexture :: TextureBuilder (Texture a) -> Graphics m (Texture a)
  DestroyTexture :: Texture a -> Graphics m ()
  GetCamera :: Graphics m Camera
  ModifyCamera :: (Camera -> Camera) -> Graphics m ()


makeSem ''Graphics

runGraphicsE :: Member (Embed IO) r => (IO () -> IO ()) -> GraphicsData -> Sem (Graphics : r) a -> Sem r a
runGraphicsE addTextureAction gd = interpret $ \case
  (Render renderTexture) -> liftIO $ modifyIORef' (dataRenderObjects gd) (:|> renderTexture)
  GetCamera -> liftIO $ readIORef (dataCamera gd)
  (ModifyCamera modifyCam) -> liftIO $ modifyIORef (dataCamera gd) modifyCam
  (DestroyTexture (Texture _ sdlTexture _)) -> liftIO $ SDL.destroyTexture sdlTexture
  (MakeTexture textureBuilder) -> liftIO $ do
    preferredFormat <- findFormat
    (texture, textureAction) <- runTextureBuilder (dataRenderer gd) preferredFormat (rgbaToPreferredFormat preferredFormat) textureBuilder
    addTextureAction textureAction
    pure texture
    where findFormat = pure SDL.ARGB8888 -- TODO: Actually find out the best format!
          rgbaToPreferredFormat SDL.ARGB8888 (r,g,b,a) = B.word8 b <> B.word8 g <> B.word8 r <> B.word8 a

runGraphics :: (Member (Embed IO) r) => SDL.Renderer -> Signal (Sem (Graphics:r)) b -> Signal (Sem r) b
runGraphics renderer signal = Signal $ do
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  graphicsData <- initGraphicsData defCamera renderer
  delayedTextureActions <- liftIO $ newIORef mempty
  renderObjectsMVar <- liftIO newEmptyMVar

  --tId <- liftIO . forkIO $ forever $ do
  --  (delayedAction, textures, camera) <- takeMVar renderObjectsMVar
  --  delayedAction
  --  renderObjects renderer textures camera
  let makeSig sig = Signal $ do
        liftIO $ writeIORef (dataRenderObjects graphicsData) mempty
        (b, cont) <- stepSignal sig
        textures <- liftIO $ readIORef (dataRenderObjects graphicsData)
        camera <- liftIO $ readIORef (dataCamera graphicsData)
        delayedAction <- liftIO $ atomicModifyIORef' delayedTextureActions $ \actions -> (mempty, actions)
        liftIO $ tryTakeMVar renderObjectsMVar >> putMVar renderObjectsMVar (delayedAction,textures,camera)
        liftIO $ delayedAction
        liftIO $ renderObjects renderer textures camera

        -- FIXME Rendering is currently done in the main thread!
        pure (b,makeSig cont)

  stepSignal (signalMorph (runGraphicsE (\newAction -> modifyIORef delayedTextureActions (>>newAction)) graphicsData) $ makeSig signal)
    where
      defCamera = Camera $ Placed (V2 0 0) $ newRectangle 800 600
      renderObjects renderer textures camera = do
        SDL.rendererRenderTarget renderer $= Nothing
        SDL.rendererDrawColor renderer $= SDL.V4 30 30 30 maxBound
        SDL.rendererLogicalSize renderer $= pure (toEnum <$> (camera ^. cameraArea . placedShape . rectangleDimensions))
        SDL.rendererClipRect renderer $= pure (transformRectangle (Placed (V2 0 0) $ camera ^. cameraArea . placedShape))
        SDL.clear renderer
        liftIO $ mapM_ runRender (sortBy (\r1 r2 -> _riZIndex r1 `compare` _riZIndex r2) textures)
        SDL.present renderer
        pure ()
          where runRender (RenderInstruction _ (Texture _ sdlTexture _) srcRect destRect rotationDegrees rotationPoint (V2 flipX flipY)) =
                  let newDestRect = destRect <&> over placedPosition (adjustPointForRender camera) . over (placedShape . rectangleHeight) negate
                  in do
                      SDL.copyEx renderer sdlTexture (transformRectangle<$>srcRect) (transformRectangle<$>newDestRect) (CDouble rotationDegrees) (SDL.P . fmap toEnum <$> rotationPoint) (V2 flipX (not flipY))



adjustPointForRender :: Camera -> V2 Int -> V2 Int
adjustPointForRender (Camera (Placed (V2 cX cY) (Rectangle _ rY))) (V2 x y) =
  V2 (x - cX) (rY - y + cY)
