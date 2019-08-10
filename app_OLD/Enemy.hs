{-# LANGUAGE TemplateHaskell #-}

module Enemy where

import Polysemy
import Polysemy.Reader
import Polysemy.State
import Sigma.Signal
import Lens.Micro
import Lens.Micro.Extras
import Lens.Micro.TH
import qualified Data.Massiv.Array as M
import Linear.V2


import Effect.GlobalState
import Effect.Graphics
import Effect.Physics
import Bullets
import StateOperation
import Shapes2D

data Enemy = Basic

data BasicEnemy = BasicEnemy
  { _basicEnemyPosition :: V2 Double
  , _basicEnemyShootCooldown :: Double
  , _basicEnemyHealth :: Double
  }

makeLenses 'BasicEnemy


handleEnemy :: Members [GlobalState Bullets, Graphics, Physics] r => Enemy -> Signal r ()
handleEnemy Basic = basicEnemy


basicEnemy :: Members [GlobalState Bullets, Graphics, Physics] r => Signal r ()
basicEnemy = feedback defaultBasicEnemy $ withInitialization ((,) <$> enemyTexture <*> enemyPhysics) $
               \((texture, freeTexture), (body, freeBody)) -> liftSem $ do
                 basicEnemy <- get
                 if basicEnemy ^. basicEnemyShootCooldown < 0
                   then do
                     spawnBullet EnemyStraight (basicEnemy^.basicEnemyPosition)
                     modify (set basicEnemyShootCooldown 1)
                   else modify (over basicEnemyShootCooldown (\x -> x-(1/60)))
                 render $ defaultRenderInstruction texture
                            & riZIndex .~ 10
                            & riTextureArea .~ Nothing
                            & riScreenArea .~ pure (Placed (round <$> basicEnemy ^. basicEnemyPosition - V2 25 25) (Rectangle 50 50))




  where
    enemyTexture :: Member Graphics r => Sem r (Texture Any, Sem r ())
    enemyTexture = fmap (\tex -> (tex, destroyTexture tex)) $ makeTexture $ do
      tex <- createStaticTexture (Rectangle 50 50)
      updateStaticTexture tex Nothing $ M.makeArray M.Seq (M.Sz2 50 50) (\(M.Ix2 x y) -> (255,20,20,255))
      pure (toAnyTexture tex)

    enemyPhysics :: Members [Physics, State BasicEnemy] r => Sem r (Body, Sem r ())
    enemyPhysics = do
      enemyBody <- createBody $ DynamicBody 50 10000
      enemyShape <- createShape enemyBody $ CircleShape (Circle 30) (V2 0 0)
      (view basicEnemyPosition <$> get) >>= \pos -> soSet pos $ bodyPosition enemyBody
      addBodyToSpace enemyBody
      addShapeToSpace enemyShape

      pure ( enemyBody, do
               freeShape enemyShape
               freeBody enemyBody
               removeShapeFromSpace enemyShape
               removeBodyFromSpace enemyBody
            )
    defaultBasicEnemy = BasicEnemy (V2 200 300) 1 10
