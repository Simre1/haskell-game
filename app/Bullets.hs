{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
module Bullets (manageBullets, BulletType(..), initializeBulletState, signalSpawnBullet, signalSpawnBullets, spawnBullet, Bullets) where

import Polysemy (Member, Sem, Members)
import Linear (V2(V2))
import Control.Arrow (arr, first)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Massiv.Array as M
import Graphics.ColorSpace
import Data.Function ((&))
import Lens.Micro ((.~))
import Polysemy.Reader (ask, Reader)
import Polysemy.State (get, put, modify)
import Polysemy (raise, Lift)
import Control.Monad.IO.Class

import Shapes2D (Placed(Placed), Rectangle(Rectangle), Circle(Circle))
import Sigma.Signal (Signal, feedback, buildSignal, withInitialization, stepSignal, liftSem, signalAsk, signalModify)
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
  , RenderInstruction
  , render
  , makeRenderInstruction
  , makeTexture
  , createStaticTexture
  , updateStaticTexture
  , toAnyTexture
  , defaultRenderInstruction
  , riZIndex
  , riScreenArea
  )
import Effect.GlobalState (GlobalState, withGlobalState, runGlobalStateWithIORef, withGlobalStateSignal)

data BulletType = Straight | EnemyStraight deriving Show

data Bullets = Bullets [(BulletType, V2 Double)] deriving Show

spawnBullet :: (Member (GlobalState Bullets) r) => BulletType -> V2 Double -> Sem r ()
spawnBullet bulletType bulletPosition = withGlobalState @Bullets $ do
  Bullets bullets <- get
  put $ Bullets $ (bulletType, bulletPosition):bullets


signalSpawnBullet :: (Member (GlobalState Bullets) r) => Signal (Reader (Maybe (BulletType, V2 Double)) : r) ()
signalSpawnBullet = liftSem $
  (ask @(Maybe (BulletType, V2 Double))) >>= maybe
    (pure ())
    (\(bulletType, bulletPosition) -> spawnBullet bulletType bulletPosition)


signalSpawnBullets :: (Member (GlobalState Bullets) r) => Signal (Reader [(BulletType, V2 Double)] : r) ()
signalSpawnBullets = withGlobalStateSignal @Bullets $
   signalModify $ (\newBullets (Bullets activeBullets) -> Bullets (newBullets ++ activeBullets)) <$> signalAsk

initializeBulletState :: Member (Lift IO) r => Signal (GlobalState Bullets : r) a -> Signal r a
initializeBulletState = runGlobalStateWithIORef (Bullets [])

manageBullets :: forall r. Members [Physics, Graphics, GlobalState Bullets] r => Signal r ()
manageBullets = feedback ([]@(Signal r (Maybe RenderInstruction))) $
  let sig = liftSem $ withGlobalState @Bullets $ do
        bulletSignals :: [Signal r (Maybe RenderInstruction)] <- get
        Bullets newBullets <- get
        let newBulletSignals =
              uncurry makeBulletSignal <$> newBullets
        aliveBullets <- fmap (first fromJust) . filter (not . null . fst) <$> sequenceA (raise . raise . stepSignal <$> (newBulletSignals ++ bulletSignals))
        sequenceA $ render . fst <$> aliveBullets
        put $ snd <$> aliveBullets
        put $ Bullets []
        pure ()

  in sig


isInside :: (Ord x, Num x) => V2 x -> Placed Rectangle x -> Bool
isInside (V2 x y) (Placed (V2 minX minY) (Rectangle areaX areaY)) =
  x >= minX && y >= minY && minX + areaX >= x && minY + areaY >= y

makeBulletSignal :: (Member Physics r, Member Graphics r) => BulletType -> V2 Double -> Signal r (Maybe RenderInstruction)
makeBulletSignal Straight initialPosition = straightBullet initialPosition
makeBulletSignal EnemyStraight initialPosition = straightEnemyBullet initialPosition


straightEnemyBullet :: (Member Physics r, Member Graphics r) => V2 Double -> Signal r (Maybe RenderInstruction)
straightEnemyBullet initialPosition = withInitialization ((,) <$> initializeBulletPhysics <*> makeBulletTexture) $ \((bulletBody, freeBullet), bulletTexture) ->
  liftSem $ do
    bulletPosition <- soGet $ bodyPosition bulletBody
    if isInside bulletPosition $ Placed (V2 0 0) $ Rectangle 480 480
      then pure . pure $ defaultRenderInstruction bulletTexture
                              & riZIndex .~ 0
                              & riScreenArea .~ (pure $ Placed (fmap round bulletPosition - V2 5 5) $ Rectangle 10 10)

      else freeBullet *> pure Nothing
  where
    initializeBulletPhysics :: Member Physics r => Sem r (Body, Sem r ())
    initializeBulletPhysics = do
          bulletBody <- createBody $ DynamicBody 1 1
          soSet initialPosition $ bodyPosition bulletBody
          bulletShape <- createShape bulletBody $ CircleShape (Circle 5) (V2 0 0)
          soSet True $ shapeSensor bulletShape
          soSet (V2 0 (-0.1)) $ bodyVelocity bulletBody
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
      updateStaticTexture texture Nothing $ M.makeArray M.Seq (M.Sz2 10 10) (const (255,255,255,255))
      pure $ toAnyTexture texture


straightBullet :: (Member Physics r, Member Graphics r) => V2 Double -> Signal r (Maybe RenderInstruction)
straightBullet initialPosition = withInitialization ((,) <$> initializeBulletPhysics <*> makeBulletTexture) $ \((bulletBody, freeBullet), bulletTexture) ->
  liftSem $ do
    bulletPosition <- soGet $ bodyPosition bulletBody
    if isInside bulletPosition $ Placed (V2 0 0) $ Rectangle 480 480
      then pure . pure $ defaultRenderInstruction bulletTexture
                              & riZIndex .~ 0
                              & riScreenArea .~ (pure $ Placed (fmap round bulletPosition - V2 5 5) $ Rectangle 10 10)

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
      updateStaticTexture texture Nothing $ M.makeArray M.Seq (M.Sz2 10 10) (const (255,255,255,255))
      pure $ toAnyTexture texture
