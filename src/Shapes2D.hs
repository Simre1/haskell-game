{-# LANGUAGE TemplateHaskell #-}

module Shapes2D
  ( module V2
  , Circle(..)
  , newCircle
  , circleRadius
  , Rectangle(..)
  , newRectangle
  , rectangleWidth
  , rectangleHeight
  , rectangleDimensions
  , Line(..)
  , lineStart
  , lineEnd
  , Placed(..)
  , placedPosition
  , placedShape
  ) where

import Linear.V2 as V2
import Linear.V4
import Lens.Micro
import Lens.Micro.TH


data Circle x = Circle {_circleRadius :: x} deriving (Eq, Functor, Show)

makeLenses ''Circle

newCircle = Circle

data Rectangle x = Rectangle {_rectangleWidth :: x, _rectangleHeight :: x} deriving (Eq, Functor, Show)

newRectangle = Rectangle

makeLenses ''Rectangle

rectangleDimensions :: Lens' (Rectangle x) (V2 x)
rectangleDimensions apply rectangle =
  (\(V2 newWidth newHeight) -> rectangle{_rectangleWidth=newWidth, _rectangleHeight=newHeight}) <$>
    apply (V2 (_rectangleWidth rectangle ) (_rectangleHeight rectangle))


data Line x = Line {_lineStart :: V2 x, _lineEnd :: V2 x} deriving (Eq, Functor, Show)

newLine = Line

makeLenses ''Line

data Placed b a = Placed {_placedPosition :: (V2 a), _placedShape :: (b a)} deriving (Eq, Functor, Show)

makeLenses ''Placed
