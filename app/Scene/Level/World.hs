{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Scene.Level.World where

import ECS.Apecs (Component(..), Map, Unique, makeWorld, Has(..), asks, SystemT(..), explInit)
import ECS.Physics (Physics)


data Player = Player Int

instance Component Player where
  type Storage Player = Unique Player


data BulletType = Straight

data Bullet = Bullet BulletType

instance Component Bullet where
  type Storage Bullet = Map Bullet

data QuantifiedSpeed = Slowest | Slow | Medium | Fast | Fastest deriving (Enum, Eq, Bounded)

data EnemyType = Argo Int QuantifiedSpeed deriving Eq

data Enemy = Enemy EnemyType

instance Component Enemy where
  type Storage Enemy = Map Enemy


makeWorld "World" [''Physics, ''Player, ''Bullet, ''Enemy]
