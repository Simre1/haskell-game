{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.TextureBuilder where

import Data.StateVar (($=))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.State (StateT(StateT), modify, runStateT)
import Lens.Micro ((^.))
import Foreign.C.Types (CInt)
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Utils (copyBytes)
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Control.Monad (guard)
import Control.Exception (SomeException)
import Linear.V2 (V2(V2))
import Linear.V4(V4(V4))
import Data.Word (Word8)
import Data.Colour (AlphaColour, over, alphaChannel)
import Data.Colour.SRGB (RGB(..), toSRGB24)

import qualified SDL
import qualified Data.Massiv.Array as M


import Graphics.Image (Image, Pixel, readImage)
import Shapes2D (Placed(..), Rectangle(..))



type PixelToBytes = Pixel -> Builder

newtype TextureBuilder a = TextureBuilder {getBuilderStack :: ReaderT (SDL.Renderer, SDL.PixelFormat, PixelToBytes) (StateT (IO ()) IO) a} deriving (Functor, Applicative, Monad)

data Texture a = Texture a SDL.Texture (Int, Int)

data Streaming = Streaming
data Static = Static
data Target = Target
data Any = Any

class ToAnyTexture a where
  toAnyTexture :: Texture a -> Texture Any

instance ToAnyTexture Any where
  toAnyTexture = id

instance ToAnyTexture Static where
  toAnyTexture (Texture _ texture textureSize) = Texture Any texture textureSize


instance ToAnyTexture Streaming where
  toAnyTexture (Texture _ texture textureSize) = Texture Any texture textureSize


instance ToAnyTexture Target where
  toAnyTexture (Texture _ texture textureSize) = Texture Any texture textureSize

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
  pure $ Texture Static tex (x,y)


createStreamingTexture :: Rectangle Int -> TextureBuilder (Texture Streaming)
createStreamingTexture (Rectangle x y) = withAll $ \renderer format _ -> do
  tex <- SDL.createTexture renderer format SDL.TextureAccessStreaming (toEnum <$> V2 x y)
  SDL.textureBlendMode tex $= SDL.BlendAlphaBlend
  pure $ Texture Streaming tex (x,y)

createTargetTexture :: Rectangle Int -> TextureBuilder (Texture Target)
createTargetTexture (Rectangle x y) = withAll $ \renderer format _ -> do
  tex <- SDL.createTexture renderer format SDL.TextureAccessTarget (toEnum <$> V2 x y)
  SDL.textureBlendMode tex $= SDL.BlendAlphaBlend
  pure $ Texture Target tex (x,y)



copy :: (Texture a, Maybe (Placed Rectangle Int)) -> (Texture Target, Maybe (Placed Rectangle Int)) -> TextureBuilder ()
copy (Texture _ srcTexture _ , srcRect) (Texture _ dstTexture _, destRect) = withRenderer $ \renderer -> delayAction $ do
  SDL.rendererRenderTarget renderer $= pure dstTexture
  SDL.copy renderer srcTexture (transformRectangle <$> srcRect) (transformRectangle <$> destRect)

updateStaticTexture :: Texture Static -> Maybe (Placed Rectangle Int) -> Image -> TextureBuilder ()
updateStaticTexture (Texture _ texture (textureX, textureY)) destRect image = withAll $ \renderer _ pixelToBytes -> delayAction $ do
  let (pixels, width, _) = imageToByteStringWithSizeChecking pixelToBytes (textureX, textureY) destRect image
  SDL.updateTexture texture (transformRectangle <$> destRect) pixels (toEnum $ width * 4)
  pure ()

updateStreamingTexture :: Texture Streaming -> Maybe (Placed Rectangle Int) -> Image -> TextureBuilder ()
updateStreamingTexture (Texture _ texture (textureX, textureY)) destRect image = withAll $ \renderer _ pixelToBytes -> delayAction $ do
  let (pixels,width,height) = imageToByteStringWithSizeChecking pixelToBytes (textureX, textureY) destRect image
  unsafeUseAsCString pixels $ \pixelBytes -> do
    (texturePtr, pitch) <- SDL.lockTexture texture (transformRectangle <$> destRect)
    let newPtr = castPtr texturePtr
    copyBytes newPtr pixelBytes (width * height * 4)
    SDL.unlockTexture texture


loadImage :: Text -> TextureBuilder (Either SomeException Image)
loadImage = withRenderer . const . liftIO . readImage


imageToByteStringWithSizeChecking :: PixelToBytes -> (Int, Int) -> Maybe (Placed Rectangle Int) -> Image -> (ByteString, Int, Int)
imageToByteStringWithSizeChecking pixelToBytes (textureWidth, textureHeight) selectedArea image =
  case selectedArea of
       Nothing -> (,textureWidth, textureHeight) $ fromMaybe emptyImage
                    (imageByteStringWithSizeCheck textureWidth textureHeight)
       (Just (Placed (V2 selectedX selectedY) (Rectangle selectedWidth selectedHeight))) ->
        fromMaybe (emptyImage, textureWidth, textureHeight) $ do
          guard $ selectedX >= 0 && selectedY >= 0
          guard $ selectedX + selectedWidth <= textureWidth && selectedY + selectedHeight <= textureHeight
          (,selectedWidth, selectedHeight) <$> imageByteStringWithSizeCheck selectedWidth selectedHeight
  where
    imageByteStringWithSizeCheck width height = do
      let (M.Sz (M.Ix2 imageWidth imageHeight)) = M.size image
      guard $ imageWidth == width && imageHeight == height
      pure $ imageToByteString pixelToBytes image
    emptyImage = toStrict . toLazyByteString $ appendBuilderNTimes (textureWidth * textureHeight) (pixelToBytes (0,0,0,255))
      where appendBuilderNTimes 0 builder = builder
            appendBuilderNTimes n builder = builder <> appendBuilderNTimes (n-1) builder

    imageToByteString :: PixelToBytes -> Image -> ByteString
    imageToByteString pixelToBytes image = toStrict $ toLazyByteString $
      let
        (M.Sz (M.Ix2 width height)) = M.size image
      in buildByteString image (width, height) (0,0)
      where
        buildByteString :: Image -> (Int, Int) -> (Int, Int) -> Builder
        buildByteString image (maxWidth,maxHeight) (currentX, currentY)
          | maxHeight == currentY = mempty
          | maxWidth == currentX = buildByteString image (maxWidth, maxHeight) (0, succ currentY)
          | otherwise = pixelToBytes (image M.! (M.Ix2 currentX currentY)) <>
                          buildByteString image (maxWidth,maxHeight) (succ currentX, currentY)

-- Transformations


transformColour :: AlphaColour Double -> V4 Word8
transformColour c = let (RGB r g b) = toSRGB24 (c `over` mempty)
                            in SDL.V4 r g b (round $ 100 * alphaChannel c)

transformRectangle :: Placed Rectangle Int -> SDL.Rectangle CInt
transformRectangle (Placed pos (Rectangle width height)) =
  SDL.Rectangle (SDL.P $ toEnum <$> pos) (toEnum <$> V2 width height)
