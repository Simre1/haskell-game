{-# LANGUAGE TemplateHaskell #-}

module Effect.Graphics.Internal where

import Import

import qualified SDL
import Data.Sequence hiding (empty)
import Shapes2D
import Sigma
import Resource

import Data.IORef
import Data.Coerce
import Data.StateVar
import Data.Word
import Control.Concurrent
import Control.Concurrent.MVar


data Camera = Camera {cameraPos :: V2 Int, cameraView :: Rectangle Int}

newCamera = Camera

data GraphicsData = GraphicsData {dataRenderObjects :: IORef (Seq (Int, RenderAction ())), dataRenderer :: SDL.Renderer, dataCamera :: IORef Camera}

initGraphicsData :: MonadIO m => Camera -> SDL.Renderer -> m (GraphicsData)
initGraphicsData (c) renderer = GraphicsData <$> liftIO (newIORef mempty) <*> pure renderer <*> liftIO (newIORef c)

data RenderAction a = RenderAction {runRenderAction :: SDL.Renderer -> Camera -> IO a} deriving (Functor)

instance Applicative RenderAction where
  pure = RenderAction . const . const . pure
  (RenderAction f1) <*> (RenderAction f2) = RenderAction $ \a x -> f1 a x <*> f2 a x

instance Monad RenderAction where
  (RenderAction f) >>= f2 = RenderAction $ \r x -> do
    a <- (f r x)
    runRenderAction (f2 a) r x

raGetCamera :: RenderAction Camera
raGetCamera = RenderAction $ \_ c -> pure c

raGetRenderer :: RenderAction SDL.Renderer
raGetRenderer = RenderAction $ \r _ -> pure r

newtype GraphicsResource a = GraphicsResource {runGraphicsResource :: SDL.Renderer -> IO a} deriving (Functor)

instance Applicative GraphicsResource where
  pure = GraphicsResource . const . pure
  (GraphicsResource f1) <*> (GraphicsResource f2) = GraphicsResource $ \a -> f1 a <*> f2 a

instance Monad GraphicsResource where
  (GraphicsResource f) >>= f2 = GraphicsResource $ \r -> do
    a <- (f r)
    runGraphicsResource (f2 a) r

data Graphics (m :: * -> *) k where
  AquireGraphicsResource :: GraphicsResource a -> Graphics m a
  Render :: Int -> RenderAction () -> Graphics m ()
  GetCamera :: Graphics m Camera
  ModifyCamera :: (Camera -> Camera) -> Graphics m ()


makeSem ''Graphics

runGraphicsE :: Member (Lift IO) r => GraphicsData -> Sem (Graphics : r) a -> Sem r a
runGraphicsE gd = interpret $ \case
  (AquireGraphicsResource res) -> liftIO $ runGraphicsResource res (dataRenderer gd)
  (Render i ro) -> liftIO $ modifyIORef' (dataRenderObjects gd) (:|> (i,ro))
  (GetCamera) -> liftIO $ readIORef (dataCamera gd)
  (ModifyCamera f) -> liftIO $ modifyIORef (dataCamera gd) f


runGraphics :: (Member (Lift IO) r) => SDL.Renderer -> Signal (Sem (Graphics:r)) a b -> Signal (Sem r) a b
runGraphics renderer signal = Signal $ \a -> do
  graphicsData <- initGraphicsData (defCamera) renderer
  renderObjectsMVar <- liftIO newEmptyMVar
  tId <- liftIO . forkIO $ let loop = takeMVar renderObjectsMVar >>= uncurry (renderObjects renderer) >> loop in loop
  let makeSig sig = Signal $ \a -> do
        liftIO $ writeIORef (dataRenderObjects graphicsData) mempty
        (b, cont) <- stepSignal sig a
        set <- liftIO $ readIORef (dataRenderObjects graphicsData)
        cam <- liftIO $ readIORef (dataCamera graphicsData)
        liftIO $ tryTakeMVar renderObjectsMVar >> putMVar renderObjectsMVar (set,cam)
        pure (b,makeSig cont)
  stepSignal (signalSimpleMorph (runGraphicsE graphicsData) $ makeSig signal) a
    where
      defCamera = newCamera (V2 0 0) $ newRectangle (V2 800 600)
      renderObjects renderer objects camera = do
        SDL.rendererRenderTarget renderer $= Nothing
        liftIO $ mapM_ runRender (sortBy (\r1 r2 -> fst r1 `compare` fst r2) objects)
        SDL.present renderer
        pure ()
          where runRender (_,ro) = runRenderAction ro renderer camera

moveCamera :: (Member Graphics e) => (V2 Int -> V2 Int) -> Sem e ()
moveCamera f = modifyCamera (\(Camera pos rect) -> Camera (f pos) rect)


adjustPointForRender :: Camera -> V2 Int -> V2 Int
adjustPointForRender (Camera (V2 cX cY) rect) (V2 x y) =
  V2 (x - cX) ((v2GetY $ rectangleDimensions rect) - y + cY)

adjustDistanceForRender :: V2 Int -> V2 Int
adjustDistanceForRender (V2 x y) =
  V2 x (-y)
--
-- adjustToCameraPosition (Camera rect) (V2 x y) =
--             let (V2 cX cY) = rectangleAnchor rect
--             in V2 (x - cX) (y - cY)
--
-- adjustYPosition (Camera rect) (V2 x y) =
--             let (V2 _ h) = rectangleDimensions rect
--             in V2 x (h - y)
--
-- inverseY (V2 x y) = (V2 x (-y))


transformRectangle :: V2 x -> Rectangle x -> SDL.Rectangle x
transformRectangle pos rect = SDL.Rectangle (SDL.P pos) (rectangleDimensions rect)

alphaColourToV4 :: AlphaColour Double -> SDL.V4 Word8
alphaColourToV4 c = let (RGB r g b) = toSRGB24 (c `over` mempty)
                            in SDL.V4 r g b (round $ 100 * alphaChannel c)

v4ToAlphaColour :: SDL.V4 Word8 -> AlphaColour Double
v4ToAlphaColour (SDL.V4 r g b a) = withOpacity (sRGB24 r g b) (fromIntegral $ a)


data RendererSettings' = RendererSettings' {setSettings :: IO (), restoreSettings :: IO ()}

instance Semigroup RendererSettings' where
  (RendererSettings' s1 r1) <> (RendererSettings' s2 r2) = RendererSettings' (s1 <> s2) (s1 <> s2)

instance Monoid RendererSettings' where
  mempty = RendererSettings' empty empty

type RendererSettings = (SDL.Renderer -> IO RendererSettings')

makeRendererSettings :: x -> (SDL.Renderer -> StateVar x) -> RendererSettings
makeRendererSettings newValue stateVar r = do
  oldValue <- Data.StateVar.get $ stateVar r
  pure $ RendererSettings' (stateVar r $= newValue) (stateVar r $= oldValue)


rendererColour :: AlphaColour Double -> RendererSettings
rendererColour colour = makeRendererSettings (alphaColourToV4 colour) SDL.rendererDrawColor


withSettings :: RendererSettings -> RenderAction a -> RenderAction a
withSettings f ra = RenderAction $ \r c -> do
  rs <- f r
  setSettings rs
  a <- runRenderAction ra r c
  restoreSettings rs
  pure a
