{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Level.World where

import ECS.Apecs (Component(..), Map, Unique, makeWorld, Has(..), asks, SystemT(..), explInit)
import ECS.Physics (Physics)


data Player = Player Double

instance Component Player where
  type Storage Player = Unique Player


data BulletType = Straight

data Bullet = Bullet BulletType

instance Component Bullet where
  type Storage Bullet = Map Bullet


data EnemyType = Argo Int deriving Eq

data Enemy = Enemy EnemyType

instance Component Enemy where
  type Storage Enemy = Map Enemy


makeWorld "World" [''Physics, ''Player, ''Bullet, ''Enemy]
