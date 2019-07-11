module Graphics.Image where

import Data.Massiv.Array
import Data.Either
import Control.Exception
import Data.Text
import Data.Word
import Foreign.Storable.Tuple
import qualified Codec.Picture as JP

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

  -- withRenderer . const . liftIO . try . IMG.readImageAuto . unpack
  -- where
  --   loadImageWithJP :: Text -> IO (Either String (IMG.Image VS RGBA Word8))
  --   loadImageWithJP filepath = do
  --     eitherImage <- fmap JP.convertRGBA8 <$> JP.readImage (unpack filepath)
  --     let i = (\img -> makeImageR VS (JP.imageWidth img, JP.imageHeight img) (\(x,y) -> let (JP.PixelRGBA8 r g b a) = JP.pixelAt img x y in PixelRGBA r g b a)) <$> eitherImage
  --     case i of
  --       Right x -> writeImage "/home/simon/test.bmp" x
  --       Left x -> error x
    --   pure i
