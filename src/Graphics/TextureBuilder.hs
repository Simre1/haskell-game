module Graphics.TextureBuilder where

import Prelude
import qualified SDL

import Linear.V2
import Linear.V4
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.StateVar
import Shapes2D
import Foreign.C
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Utils
import Data.Word
import Data.Colour
import Data.Colour.SRGB
import Graphics.Image as IMG
import qualified Graphics.Image.Interface as IMG
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe
import Lens.Micro hiding (over)
import Lens.Micro.Extras
import Data.Maybe

type PixelToBytes = (Word8, Word8, Word8, Word8) -> B.Builder

newtype TextureBuilder a = TextureBuilder {getBuilderStack :: ReaderT (SDL.Renderer, SDL.PixelFormat, PixelToBytes) (StateT (IO ()) IO) a} deriving (Functor, Applicative, Monad)



data Texture a = Texture a SDL.Texture

data Streaming = Streaming
data Static = Static
data Target = Target

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

createStaticTexture :: V2 Int -> TextureBuilder (Texture Static)
createStaticTexture (V2 x y) = withAll $ \renderer format _ -> do
  tex <- SDL.createTexture renderer format SDL.TextureAccessStatic (toEnum <$> V2 x y)
  SDL.textureBlendMode tex $= SDL.BlendAlphaBlend
  pure $ Texture Static tex


createStreamingTexture :: V2 Int -> TextureBuilder (Texture Streaming)
createStreamingTexture (V2 x y) = withAll $ \renderer format _ -> do
  tex <- SDL.createTexture renderer format SDL.TextureAccessStreaming (toEnum <$> V2 x y)
  SDL.textureBlendMode tex $= SDL.BlendAlphaBlend
  pure $ Texture Streaming tex

createTargetTexture :: V2 Int -> TextureBuilder (Texture Target)
createTargetTexture (V2 x y) = withAll $ \renderer format _ -> do
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


updateStaticTexture :: MArray arr RGBA Word8 => Texture Static -> Maybe (Placed Rectangle Int) -> Image arr RGBA Word8 -> TextureBuilder ()
updateStaticTexture (Texture _ texture) destRect image = withAll $ \renderer _ pixelToBytes -> delayAction $ do
  (areaX, areaY) <- textureSelectedAreaDimensions texture destRect
  let pixels = imageWithFixedSizeToByteString pixelToBytes (areaX, areaY) image
  SDL.updateTexture texture (transformRectangle <$> destRect) pixels (toEnum $ areaX * 4)
  pure ()
  where byteString pixelToBytes = BL.toStrict $ B.toLazyByteString $ IMG.foldl (accume pixelToBytes) mempty image
        accume pixelToBytes builder pixel = builder <> (\pixel -> pixelToBytes pixel) (IMG.toComponents pixel)

updateStreamingTexture :: MArray arr RGBA Word8 => Texture Streaming -> Maybe (Placed Rectangle Int) -> Image arr RGBA Word8 -> TextureBuilder ()
updateStreamingTexture (Texture _ texture) destRect image = withAll $ \renderer _ pixelToBytes -> delayAction $ do
  (areaX, areaY) <- textureSelectedAreaDimensions texture destRect
  let pixels = imageWithFixedSizeToByteString pixelToBytes (areaX, areaY) image
  unsafeUseAsCString pixels $ \pixelBytes -> do
    (texturePtr, pitch) <- SDL.lockTexture texture (transformRectangle <$> destRect)
    let newPtr = castPtr texturePtr
    copyBytes newPtr pixelBytes (4 * areaX * areaY)
    SDL.unlockTexture texture

imageWithFixedSizeToByteString :: MArray arr RGBA Word8 => PixelToBytes -> (Int, Int) -> Image arr RGBA Word8 -> BS.ByteString
imageWithFixedSizeToByteString pixelToBytes (width, height) image =
  let (imageWidth, imageHeight) = dims image
  in if (imageWidth == width && imageHeight == height)
      then imageToByteString pixelToBytes image
      else BL.toStrict $ B.toLazyByteString $ appendBuilderNTimes (width * height) (pixelToBytes (0,0,0,255))
  where appendBuilderNTimes 0 builder = builder
        appendBuilderNTimes n builder = builder <> appendBuilderNTimes (n-1) builder

-- Transformations

textureSelectedAreaDimensions :: SDL.Texture -> Maybe (Placed Rectangle Int) -> IO (Int, Int)
textureSelectedAreaDimensions texture rect = case (view (placedShape . rectangleDimensions) <$> rect) of
  Just (V2 x y) -> pure (x, y)
  Nothing -> do
    textureInfo <- SDL.queryTexture texture
    pure (fromEnum $ SDL.textureWidth textureInfo, fromEnum $ SDL.textureHeight textureInfo)

imageToByteString :: MArray arr RGBA Word8 => PixelToBytes -> Image arr RGBA Word8 -> BS.ByteString
imageToByteString pixelToBytes image = BL.toStrict $ B.toLazyByteString $ IMG.foldl accume mempty image
  where accume builder pixel = builder <> pixelToBytes (IMG.toComponents pixel)

transformColour :: AlphaColour Double -> V4 Word8
transformColour c = let (RGB r g b) = toSRGB24 (c `over` mempty)
                            in SDL.V4 r g b (round $ 100 * alphaChannel c)

transformRectangle :: Placed Rectangle Int -> SDL.Rectangle CInt
transformRectangle (Placed pos (Rectangle dimensions)) =
  SDL.Rectangle (SDL.P $ toEnum <$> pos) (toEnum <$> dimensions)
