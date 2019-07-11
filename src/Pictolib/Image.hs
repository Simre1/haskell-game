module Pictolib.Image where

data RGBA e = RGBA e e e e

newtype Pixel cs e = Pixel (cs e)

newtype Image arr cs e = Image (arr (cs e))

class ImageArray
