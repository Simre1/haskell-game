module GPipe.Image where

import Data.Either
import Control.Exception
import Data.Text
import Data.Word
import Foreign.Storable.Tuple
import qualified Codec.Picture as JP
import Linear.V2
import Data.Function ((&))
import System.Endian
import Data.ByteString.Conversion
import qualified Data.Vector.Storable as V
import Data.Bits
import Control.Arrow
import Debug.Trace

import Linear.V4

type Pixel = (Word8,Word8,Word8,Word8)

data Image a = Image (V2 Int) (V.Vector a)
--
readImage :: Text -> IO (Image Word8)
readImage filepath = do
  eitherImage <- JP.readImage (unpack filepath)
  case eitherImage of
    Left err -> error err
    Right img -> do
      let rgba8Img = JP.convertRGBA8 img
          (width, height) = (JP.imageWidth rgba8Img, JP.imageHeight rgba8Img)
          imgData = JP.imageData rgba8Img
      pure $ Image (V2 width height) (imgData)


readImage2 :: Text -> IO (Image (V4 Word8))
readImage2 filepath = do
  eitherImage <- JP.readImage (unpack filepath)
  case eitherImage of
    Left err -> error err
    Right img -> do
      let rgba8Img = JP.convertRGBA8 img
          (width, height) = (JP.imageWidth rgba8Img, JP.imageHeight rgba8Img)
          imgData = JP.imageData rgba8Img
      pure $ Image (V2 width height) (toWord32Vector imgData)
  where toWord32Vector = V.unfoldr $ \remainingVector ->
          if V.length remainingVector >= 4
            then Just . first word8ToV4 $ V.splitAt 4 remainingVector
            else Nothing

word8ToV4 :: V.Vector Word8 -> V4 Word8
word8ToV4 v = V4 (v V.! 0) (v V.! 1) (v V.! 2) (v V.! 3)


word8ToWord32 :: V.Vector Word8 -> Word32
word8ToWord32 v = fromLE32 $ V.foldr accum 0 v
  where
    accum o a = (a `shiftL` 8) .|. fromIntegral o
