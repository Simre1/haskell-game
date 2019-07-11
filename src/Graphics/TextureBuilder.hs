module Graphics.TextureBuilder where

import Prelude
import qualified SDL

import Linear.V2
import Linear.V4
import Control.Exception
import Data.Text (Text, unpack)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.StateVar
import Shapes2D
import Data.Either
import Foreign.C
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Utils
import Data.Word
import Data.Colour
import Data.Colour.SRGB
import Control.Monad.IO.Class
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe
import Lens.Micro hiding (over)
import Lens.Micro.Extras
import Data.Maybe
import Data.Vector.Storable as V hiding (modify)
import Debug.Trace

import Data.Massiv.Array as M hiding (modify)
import Data.Massiv.Array.Manifest as M
import Data.Massiv.Core.Index as M

import Graphics.Image

type PixelToBytes = Pixel -> B.Builder

newtype TextureBuilder a = TextureBuilder {getBuilderStack :: ReaderT (SDL.Renderer, SDL.PixelFormat, PixelToBytes) (StateT (IO ()) IO) a} deriving (Functor, Applicative, Monad)

data Texture a = Texture a SDL.Texture

data Streaming = Streaming
data Static = Static
data Target = Target
data Any = Any

class ToAnyTexture a where
  toAnyTexture :: Texture a -> Texture Any

instance ToAnyTexture Any where
  toAnyTexture = id

instance ToAnyTexture Static where
  toAnyTexture (Texture _ texture) = Texture Any texture


instance ToAnyTexture Streaming where
  toAnyTexture (Texture _ texture) = Texture Any texture


instance ToAnyTexture Target where
  toAnyTexture (Texture _ texture) = Texture Any texture

runTextureBuilder :: SDL.Renderer -> SDL.PixelFormat -> PixelToBytes -> TextureBuilder (Texture a) -> IO (Texture a, IO ())
runTextureBuilder renderer format pixelToBytes textureBuilder = do
  texture <- runStateT (runReaderT (getBuilderStack textureBuilder) (renderer, format, pixelToBytes)) mempty
  pure texture

withRenderer :: (SDL.Renderer -> StateT (IO ()) IO a) -> TextureBuilder a
withRenderer action = TextureBuilder . ReaderT $ \(renderer,_,_) -> action renderer

withAll :: (SDL.Renderer -> SDL.PixelFormat -> PixelToBytes -> StateT (IO ()) IO a) -> TextureBuilder a
withAll action = TextureBuilder . ReaderT $ \(renderer, pixelFormat, pixelToBytes) -> action renderer pixelFormat pixelToBytes

delayAction :: IO () -> StateT (IO ()) IO ()
delayAction io = modify (>>io)

createStaticTexture :: Rectangle Int -> TextureBuilder (Texture Static)
createStaticTexture (Rectangle x y) = withAll $ \renderer format _ -> do
  tex <- SDL.createTexture renderer format SDL.TextureAccessStatic (toEnum <$> V2 x y)
  SDL.textureBlendMode tex $= SDL.BlendAlphaBlend
  pure $ Texture Static tex


createStreamingTexture :: Rectangle Int -> TextureBuilder (Texture Streaming)
createStreamingTexture (Rectangle x y) = withAll $ \renderer format _ -> do
  tex <- SDL.createTexture renderer format SDL.TextureAccessStreaming (toEnum <$> V2 x y)
  SDL.textureBlendMode tex $= SDL.BlendAlphaBlend
  pure $ Texture Streaming tex

createTargetTexture :: Rectangle Int -> TextureBuilder (Texture Target)
createTargetTexture (Rectangle x y) = withAll $ \renderer format _ -> do
  tex <- SDL.createTexture renderer format SDL.TextureAccessTarget (toEnum <$> V2 x y)
  SDL.textureBlendMode tex $= SDL.BlendAlphaBlend
  pure $ Texture Target tex

copy :: (Texture a, Maybe (Placed Rectangle Int)) -> (Texture Target, Maybe (Placed Rectangle Int)) -> TextureBuilder ()
copy (Texture _ srcTexture, srcRect) (Texture _ dstTexture, destRect) = withRenderer $ \renderer -> delayAction $ do
  SDL.rendererRenderTarget renderer $= pure dstTexture
  SDL.copy renderer srcTexture (transformRectangle <$> srcRect) (transformRectangle <$> destRect)

filledRectangle :: Maybe (Placed Rectangle Int) -> AlphaColour Double -> Texture Target -> TextureBuilder ()
filledRectangle dstRectangle colour (Texture _ destTexture) = withRenderer $ \renderer -> delayAction $ do
  SDL.rendererRenderTarget renderer $= pure destTexture
  SDL.rendererDrawColor renderer $= transformColour colour
  SDL.fillRect renderer (transformRectangle <$> dstRectangle)


textureSelectedAreaDimensions :: SDL.Texture -> Maybe (Placed Rectangle Int) -> IO (Int, Int)
textureSelectedAreaDimensions texture rect = case (view placedShape <$> rect) of
  Just (Rectangle x y) -> pure (x, y)
  Nothing -> do
    textureInfo <- SDL.queryTexture texture
    pure (fromEnum $ SDL.textureWidth textureInfo, fromEnum $ SDL.textureHeight textureInfo)


transformColour :: AlphaColour Double -> V4 Word8
transformColour c = let (RGB r g b) = toSRGB24 (c `over` mempty)
                            in SDL.V4 r g b (round $ 100 * alphaChannel c)

transformRectangle :: Placed Rectangle Int -> SDL.Rectangle CInt
transformRectangle (Placed pos (Rectangle width height)) =
  SDL.Rectangle (SDL.P $ toEnum <$> pos) (toEnum <$> V2 width height)

loadImage :: Text -> TextureBuilder (Either SomeException Image)
loadImage = withRenderer . const . liftIO . readImage

updateStaticTexture :: Texture Static -> Maybe (Placed Rectangle Int) -> Image -> TextureBuilder ()
updateStaticTexture (Texture _ texture) destRect image = withAll $ \renderer _ pixelToBytes -> delayAction $ do
  (areaX, areaY) <- textureSelectedAreaDimensions texture destRect
  let pixels = imageWithFixedSizeToByteString pixelToBytes (areaX, areaY) image
  SDL.updateTexture texture (transformRectangle <$> destRect) pixels (toEnum $ areaX * 4)
  pure ()

fourBytes :: BS.ByteString
fourBytes = BL.toStrict $ B.toLazyByteString $
     (B.word8 0 <> B.word8 0 <> B.word8 0 <> B.word8 255)--
  <> (B.word8 80 <> B.word8 80 <> B.word8 80 <> B.word8 255)
  <> (B.word8 160 <> B.word8 160 <> B.word8 160 <> B.word8 255)
  <> (B.word8 255 <> B.word8 255 <> B.word8 255 <> B.word8 255)

updateStreamingTexture :: Texture Streaming -> Maybe (Placed Rectangle Int) -> Image -> TextureBuilder ()
updateStreamingTexture (Texture _ texture) destRect image = withAll $ \renderer _ pixelToBytes -> delayAction $ do
  (areaX, areaY) <- textureSelectedAreaDimensions texture destRect
  let pixels = imageWithFixedSizeToByteString pixelToBytes (areaX, areaY) image
  unsafeUseAsCString pixels $ \pixelBytes -> do
    (texturePtr, pitch) <- SDL.lockTexture texture (transformRectangle <$> destRect)
    let newPtr = castPtr texturePtr
    copyBytes newPtr pixelBytes (4 * areaX * areaY)
    SDL.unlockTexture texture

imageWithFixedSizeToByteString :: PixelToBytes -> (Int, Int) -> Image -> BS.ByteString
imageWithFixedSizeToByteString pixelToBytes (width, height) image =
  let (Sz2 imageWidth imageHeight) = size image
  in if (imageWidth == width && imageHeight == height)
      then imageToByteString pixelToBytes image
      else BL.toStrict $ B.toLazyByteString $ appendBuilderNTimes (width * height) (pixelToBytes (0,0,0,255))
  where appendBuilderNTimes 0 builder = builder
        appendBuilderNTimes n builder = builder <> appendBuilderNTimes (n-1) builder

-- Transformations

imageToByteString :: PixelToBytes -> Image -> BS.ByteString
imageToByteString pixelToBytes image = BL.toStrict $ B.toLazyByteString $
  let
    (Sz2 width height) = size image
  in buildByteString image (width, height) (0,0)
  where
    buildByteString image (maxWidth,maxHeight) (currentX, currentY)
      | maxHeight == currentY = mempty
      | maxWidth == currentX = buildByteString image (maxWidth, maxHeight) (0, succ currentY)
      | otherwise = pixelToBytes (image M.! (M.Ix2 currentX currentY)) <>
                      buildByteString image (maxWidth,maxHeight) (succ currentX, currentY)
