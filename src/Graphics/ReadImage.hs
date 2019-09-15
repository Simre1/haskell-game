{-# LANGUAGE ExistentialQuantification #-}

module Graphics.ReadImage
  ( Image(..)
  , readImageRGBA8
  , readImageGrayscale
  )
  where


import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import Control.Arrow (first)
import Data.Text (Text, unpack)
import Data.Tuple (swap)
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import Linear.V2 (V2(V2))
import Linear.V4 (V4(V4))



data Image a = V.Storable a => Image (V.Vector a)

instance V.Storable a => Semigroup (Image a) where
  (Image imgData1) <> (Image imgData2) = Image (imgData1 <> imgData2)

instance V.Storable a => Monoid (Image a) where
  mempty = Image mempty

instance Foldable Image where
  foldr f acc (Image imgData) = V.foldr f acc imgData


readImageRGBA8 :: Text -> IO (Image (V4 Word8))
readImageRGBA8 filepath = do
  eitherImage <- JP.readImage (unpack filepath)
  case eitherImage of
    Left err -> error err
    Right img -> do
      let rgba8Img = JP.convertRGBA8 img
          (width, height) = (JP.imageWidth rgba8Img, JP.imageHeight rgba8Img)
          imgData = JP.imageData rgba8Img
      pure $ Image (toV4Vector imgData)
  where toV4Vector = V.unfoldr $ \remainingVector ->

          let l = V.length remainingVector in if l >= 4
            then Just . first (vectorToV4 l) . swap $ V.splitAt (l-4) remainingVector
            else Nothing
          where vectorToV4 l v = V4 (v V.! 0) (v V.! 1) (v V.! 2) (v V.! 3)

readImageGrayscale :: Text -> IO (Image Float)
readImageGrayscale filepath = do
  eitherImage <- JP.readImage (unpack filepath)
  case eitherImage of
    Left err -> error err
    Right dynamicImg -> case dynamicImg of
      (JP.ImageY8 img) -> do
        let (width, height) = (JP.imageWidth img, JP.imageHeight img)
            imgData = JP.imageData img
        pure $ Image (V.reverse $ V.map ((/255) . fromIntegral) imgData)
      _ -> error "That image is not grayscale!"
