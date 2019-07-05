{-# LANGUAGE UndecidableInstances #-}

module Effect.Graphics.Internal where

import Import

import qualified SDL
import Data.Sequence
import Shapes2D
import Sigma
import Resource

import Data.IORef
import Data.Coerce
import Control.Effect.Reader
import Data.StateVar
import Data.Word
import Control.Concurrent
import Control.Concurrent.MVar

data Camera = Camera {cameraRectangle :: Rectangle Int}

newCamera = Camera

cameraToView :: Camera -> View
cameraToView (Camera rect) =
  let bb = getBounds rect
  in View bb (boundingBoxSize bb)

data View = View (BoundingBox Int) (V2 Int)

isInView :: View -> BoundingBox Int -> Bool
isInView (View bb _) bb2 = boundingBoxCheckIntersection bb bb2

invertCoordinates :: View -> V2 Int -> V2 Int
invertCoordinates (View _ (V2 vX vY)) (V2 x y) = V2 x (vY - y)


data GraphicsData = GraphicsData {dataRenderObjects :: (IORef (Seq (Int, RenderAction ()))), dataRenderer :: SDL.Renderer, dataCamera :: (IORef Camera)}

runGraphicsC :: GraphicsData -> GraphicsC m a -> m a
runGraphicsC ref (GraphicsC reader) = runReaderC reader ref

initGraphicsData :: MonadIO m => (Camera) -> SDL.Renderer -> m (GraphicsData)
initGraphicsData (c) renderer = GraphicsData <$> liftIO (newIORef mempty) <*> pure renderer <*> liftIO (newIORef c)

newtype GraphicsC m a = GraphicsC (ReaderC GraphicsData m a) deriving (Functor, Applicative, Monad, MonadIO)

data RenderAction a = RenderAction {runRenderAction :: SDL.Renderer -> Camera -> IO a} deriving (Functor)

instance Applicative RenderAction where
  pure = RenderAction . const . const . pure
  (RenderAction f1) <*> (RenderAction f2) = RenderAction $ \a x -> f1 a x <*> f2 a x

instance Monad RenderAction where
  (RenderAction f) >>= f2 = RenderAction $ \r x -> do
    a <- (f r x)
    runRenderAction (f2 a) r x


newtype GraphicsResource a = GraphicsResource {runGraphicsResource :: SDL.Renderer -> IO a} deriving (Functor)

instance Applicative GraphicsResource where
  pure = GraphicsResource . const . pure
  (GraphicsResource f1) <*> (GraphicsResource f2) = GraphicsResource $ \a -> f1 a <*> f2 a

instance Monad GraphicsResource where
  (GraphicsResource f) >>= f2 = GraphicsResource $ \r -> do
    a <- (f r)
    runGraphicsResource (f2 a) r

data Graphics (m :: * -> *) k where
  AquireGraphicsResource :: GraphicsResource a -> (a -> k) -> Graphics m k
  Render :: Int -> RenderAction () -> k -> Graphics m k
  GetCamera :: (Camera -> k) -> Graphics m k
  ModifyCamera :: (Camera -> Camera) -> k -> Graphics m k


instance Functor (Graphics m) where
  fmap f (AquireGraphicsResource a k) = AquireGraphicsResource a (f . k)
  fmap f (Render i a k) = Render i a (f k)
  fmap f (GetCamera k) = GetCamera (f . k)
  fmap f (ModifyCamera a k) = ModifyCamera a (f k)

instance HFunctor Graphics where
  hmap _ = coerce

instance Effect Graphics where
  handle state handler = coerce . fmap (handler . (<$ state))

instance (Carrier sig m, Effect sig, MonadIO m) => Carrier (Graphics :+: sig) (GraphicsC m) where
  eff = \case
    (L (AquireGraphicsResource gr k)) -> GraphicsC (dataRenderer <$> ask) >>= liftIO . runGraphicsResource gr >>= k
    (L (Render i ro k)) -> do
      ref <- dataRenderObjects <$> GraphicsC ask
      liftIO $ modifyIORef' ref (:|> (i,ro))
      k
    (L (GetCamera k)) -> GraphicsC (dataCamera <$> ask) >>= liftIO . readIORef >>= k
    (L (ModifyCamera f k)) -> do
      (cRef) <- GraphicsC (dataCamera <$> ask)
      liftIO $ modifyIORef cRef f
      k
    (R other) -> GraphicsC $ eff $ R $ handleCoercible other

render :: (Member Graphics sig, Carrier sig m) => Int -> RenderAction () -> m ()
render i ro = send $ Render i ro (pure ())

aquireGraphicsResource :: (Member Graphics sig, Carrier sig m) => Resource GraphicsResource a -> Resource m a
aquireGraphicsResource gr = hmap f gr
  where f a = send $ AquireGraphicsResource a pure

modifyCamera :: (Member Graphics sig, Carrier sig m) => (Camera -> Camera) -> m ()
modifyCamera f = send $ ModifyCamera f (pure ())

getCamera :: (Member Graphics sig, Carrier sig m) => m Camera
getCamera = send $ GetCamera pure

moveCamera :: (Member Graphics sig, Carrier sig m) => (V2 Int -> V2 Int) -> m ()
moveCamera f = modifyCamera (\(Camera rect) -> Camera $ rect {rectangleAnchor=f (rectangleAnchor rect)})

runGraphics :: (Effect sig, Carrier sig m, MonadIO m) => SDL.Renderer -> Signal (GraphicsC m) a b -> Signal m a b
runGraphics renderer signal = Signal $ \a -> do
  graphicsData <- initGraphicsData (defCamera) renderer
  renderObjectsMVar <- liftIO newEmptyMVar
  tId <- liftIO . forkIO $ let loop = takeMVar renderObjectsMVar >>= uncurry (renderObjects renderer) >> loop in loop
  let makeSig sig = Signal $ \a -> do
        GraphicsData ros ren c <- GraphicsC ask
        liftIO $ writeIORef ros mempty
        (b, cont) <- stepSignal sig a
        set <- liftIO $ readIORef ros
        cam <- liftIO $ readIORef c
        liftIO $ tryTakeMVar renderObjectsMVar >> putMVar renderObjectsMVar (set,cam)
        return (b,makeSig cont)
  stepSignal (signalSimpleMorph (runGraphicsC graphicsData) $ makeSig signal) a
    where defCamera = newCamera $ newRectangle (V2 0 0) (V2 800 600)

renderObjects :: MonadIO m => SDL.Renderer -> Seq (Int, RenderAction ())  -> Camera -> m ()
renderObjects renderer set c = do
  SDL.rendererRenderTarget renderer $= Nothing
  liftIO $ mapM_ (r c) (sortBy (\r1 r2 -> fst r1 `compare` fst r2) set)
  SDL.present renderer
  return ()
    where r c (_,ro) = runRenderAction ro renderer c

adjustToCameraPosition (Camera rect) (V2 x y) =
            let (V2 cX cY) = rectangleAnchor rect
            in V2 (x - cX) (y - cY)

adjustYPosition (Camera rect) (V2 x y) =
            let (V2 _ h) = rectangleDimensions rect
            in V2 x (h - y)

inverseY (V2 x y) = (V2 x (-y))


transformRectangle :: Rectangle x -> SDL.Rectangle x
transformRectangle rect = SDL.Rectangle (SDL.P (rectangleAnchor rect)) (rectangleDimensions rect)

alphaColourToV4 :: AlphaColour Double -> SDL.V4 Word8
alphaColourToV4 c = let (RGB r g b) = toSRGB24 (c `over` mempty)
                            in SDL.V4 r g b (round $ 100 * alphaChannel c)

v4ToAlphaColour :: SDL.V4 Word8 -> AlphaColour Double
v4ToAlphaColour (SDL.V4 r g b a) = withOpacity (sRGB24 r g b) (fromIntegral $ a)

