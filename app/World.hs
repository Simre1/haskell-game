{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TemplateHaskell #-}

module World where

import Apecs
import Apecs.Physics

data Player = Player

instance Component Player where
  type Storage Player = Map Player


makeWorld "World" [''Physics, ''Player]
