module Bullets (manageBullets, BulletType(..)) where

import Polysemy (Member, Sem)
import Linear (V2(V2))
import Control.Arrow (arr, first)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Massiv.Array as M
import Graphics.ColorSpace

import Shapes2D (Placed(Placed), Rectangle(Rectangle), Circle(Circle))
import Sigma.Signal (Signal, feedback, buildSignal, withInitialization, arrM, stepSignal)
import Effect.Physics
  ( Physics
  , Body
  , bodyPosition
  , createBody
  , createShape
  , removeBodyFromSpace
  , addBodyToSpace
  , removeShapeFromSpace
  , addShapeToSpace
  , shapeSensor
  , bodyVelocity
  , freeShape
  , freeBody
  , BodyType (DynamicBody)
  , ShapeType (CircleShape)
  )
import StateOperation (soGet, soSet)
import Effect.Graphics
  ( Graphics
  , Texture
  , Any
  , render
  , makeRenderInstruction
  , makeTexture
  , createStaticTexture
  , updateStaticTexture
  , toAnyTexture
  )

data BulletType = Straight

manageBullets :: (Member Physics r, Member Graphics r) => Int -> Signal (Sem r) (Maybe (BulletType, V2 Double, Placed Rectangle Double)) ()
manageBullets zIndex = feedback [] $
  let sig = buildSignal $ \(maybeShootNewBullet, bullets) -> do
        let maybeNewBullet = maybeShootNewBullet <&> \(bulletType, bulletSpawnPosition, bulletAliveArea) ->
              makeBulletSignal bulletType bulletSpawnPosition bulletAliveArea
        maybeBullets <- sequenceA $ flip stepSignal () <$> (fromMaybe (arr $ const Nothing) maybeNewBullet : bullets)

        let aliveBullets = first fromJust <$> filter (not . null . fst) maybeBullets
        sequenceA $ renderBullets . fst <$> aliveBullets
        pure (((),snd <$> aliveBullets),sig)
  in sig
  where
    renderBullets :: Member Graphics r => (V2 Double, Texture Any) -> Sem r ()
    renderBullets (pos,texture) =
          render $ makeRenderInstruction zIndex texture Nothing (pure $ Placed (fmap round pos - V2 5 5) $ Rectangle 10 10)

isInside :: (Ord x, Num x) => V2 x -> Placed Rectangle x -> Bool
isInside (V2 x y) (Placed (V2 minX minY) (Rectangle areaX areaY)) =
  x >= minX && y >= minY && minX + areaX >= x && minY + areaY >= y

makeBulletSignal :: (Member Physics r, Member Graphics r) => BulletType -> V2 Double -> Placed Rectangle Double -> Signal (Sem r) () (Maybe (V2 Double, Texture Any))
makeBulletSignal Straight initialPosition aliveArea = withInitialization ((,) <$> initializeBulletPhysics <*> makeBulletTexture) $ \((bulletBody, freeBullet), bulletTexture) ->
  arrM . const $ do
    bulletPosition <- soGet $ bodyPosition bulletBody
    if isInside bulletPosition aliveArea
      then pure $ pure (bulletPosition, bulletTexture)
      else freeBullet *> pure Nothing
  where
    initializeBulletPhysics :: Member Physics r => Sem r (Body, Sem r ())
    initializeBulletPhysics = do
          bulletBody <- createBody $ DynamicBody 1 1
          soSet initialPosition $ bodyPosition bulletBody
          bulletShape <- createShape bulletBody $ CircleShape (Circle 5) (V2 0 0)
          soSet True $ shapeSensor bulletShape
          soSet (V2 0 0.1) $ bodyVelocity bulletBody
          addBodyToSpace bulletBody
          addShapeToSpace bulletShape
          pure (bulletBody, do
                              removeShapeFromSpace bulletShape
                              removeBodyFromSpace bulletBody
                              freeShape bulletShape
                              freeBody bulletBody

               )
    makeBulletTexture :: Member Graphics r => Sem r (Texture Any)
    makeBulletTexture = makeTexture $ do
      texture <- createStaticTexture $ Rectangle 10 10
      updateStaticTexture texture Nothing $ M.makeArray M.Seq (M.Sz2 10 10) (const (0,0,0,255))
      pure $ toAnyTexture texture
