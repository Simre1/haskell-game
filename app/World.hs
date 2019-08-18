{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TemplateHaskell #-}

module World where

import Apecs
import Apecs.Physics
import System.Random


data Player = Player Double

instance Component Player where
  type Storage Player = Unique Player


data BulletType = Straight

data Bullet = Bullet BulletType

instance Component Bullet where
  type Storage Bullet = Map Bullet


data EnemyType = Argo Int | Runex Int (Double, Double) deriving Eq

data Enemy = Enemy EnemyType

instance Component Enemy where
  type Storage Enemy = Map Enemy

data ScenarioTimer = ScenarioTimer Int Int StdGen

instance Component ScenarioTimer where
  type Storage ScenarioTimer = Unique ScenarioTimer

makeWorld "World" [''Physics, ''Player, ''Bullet, ''Enemy, ''ScenarioTimer]
