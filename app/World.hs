{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TemplateHaskell #-}

module World where

import Apecs
import Apecs.Physics

data Player = Player Double

instance Component Player where
  type Storage Player = Unique Player

data BulletType = Straight

data Bullet = Bullet BulletType

instance Component Bullet where
  type Storage Bullet = Map Bullet

makeWorld "World" [''Physics, ''Player, ''Bullet]
