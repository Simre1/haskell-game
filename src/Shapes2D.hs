{-# LANGUAGE TemplateHaskell #-}

module Shapes2D
  ( module V2
  , Circle(..)
  , newCircle
  , circleRadius
  , Rectangle(..)
  , newRectangle
  , rectangleDimensions
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

data Rectangle x = Rectangle {_rectangleDimensions :: V2 x} deriving (Eq, Functor, Show)

newRectangle = Rectangle

makeLenses ''Rectangle

data Placed b a = Placed {_placedPosition :: (V2 a), _placedShape :: (b a)} deriving (Eq, Functor, Show)

makeLenses ''Placed
