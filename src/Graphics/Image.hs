module Graphics.Image where

import Data.Massiv.Array
import Data.Either
import Control.Exception
import Data.Text
import Data.Word
import Foreign.Storable.Tuple
import qualified Codec.Picture as JP
import Linear.V2
import Data.Function ((&))

type Pixel = (Word8,Word8,Word8,Word8)

type Image = Array S Ix2 Pixel

readImage :: Text -> IO (Either SomeException Image)
readImage filepath = do
  eitherImage <- JP.readImage (unpack filepath)
  try $ case eitherImage of
    Left err -> error err
    Right img -> do
      let
        rgba8Img = JP.convertRGBA8 img
        (width, height) = (JP.imageWidth rgba8Img, JP.imageHeight rgba8Img)

      pure $ makeArray Seq (Sz2 width height) $ (\(Ix2 x y) -> let (JP.PixelRGBA8 r g b a) = JP.pixelAt rgba8Img x y  in (r,g,b,a))

flipImage :: V2 Bool -> Image -> Image
flipImage (V2 flipX flipY) img = computeAs S $ img & transform' ((,()) . id)
  (\_ getVal currentIx2 -> getVal (calcNewIx2 currentIx2))
  where (Sz (Ix2 width height)) = size img
        calcNewIx2 (Ix2 x y)
          | flipX && flipY = (Ix2 (width - succ x) (height - succ y))
          | flipX = Ix2 (width - succ x) y
          | flipY = Ix2 x (height - succ y)
          | otherwise = Ix2 x y
