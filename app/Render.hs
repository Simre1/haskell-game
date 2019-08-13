{-# LANGUAGE BangPatterns #-}

module Render where

import Effect.Apecs
import Apecs.Physics hiding (ask, get)
import Control.Monad.IO.Class
import Polysemy as P
import Polysemy.Reader
import Polysemy.Input
import Effect.Graphics
import Sigma.Signal
import Debug.Trace
import Polysemy.State
import Data.Function ((&))

import Shapes2D
import Data.Massiv.Array as M hiding ((!))
import Control.Exception
import Control.Monad

import World
import Player
import Bullets
import Enemies


renderWorld :: P.Members [(ApecsSystem World), Embed IO, Graphics] r => Signal (Sem r) ()
renderWorld = renderPlayer *> renderBullets *> renderEnemies

renderEnemies :: P.Members [(ApecsSystem World), Embed IO, Graphics] r => Signal (Sem r) ()
renderEnemies = withInitialization loadEnemiesTexture $ \texture -> liftSem $
  forEachEnemy (renderEnemy texture)
  where
    renderEnemy :: Member Graphics r => Texture Any -> EnemyType -> V2 Double -> Sem r ()
    renderEnemy texture (Argo _) pos =
      render $ makeRenderInstruction
                8
                texture
                Nothing
                (Just $ Placed (round <$> pos - V2 20 20) $ Rectangle 40 40)
                0
                Nothing
                (V2 False False)
    renderEnemy texture (Runex _ _) pos =
      render $ makeRenderInstruction
                8
                texture
                Nothing
                (Just $ Placed (round <$> pos - V2 20 20) $ Rectangle 40 40)
                0
                Nothing
                (V2 False False)

loadEnemiesTexture :: Member Graphics r => Sem r (Texture Any)
loadEnemiesTexture = makeTexture $ do
  texture <- createStaticTexture $ Rectangle 40 40
  let img = makeArray Seq (Sz (Ix2 40 40)) $ \(Ix2 x y) -> let v = toEnum $ 3 * (30 - abs (20 - x)) + 3 * (30 - abs (20 - y)) in (v,v,v,255)
  updateStaticTexture texture Nothing img
  pure $ toAnyTexture texture


renderBullets :: P.Members [(ApecsSystem World), Embed IO, Graphics] r => Signal (Sem r) ()
renderBullets = withInitialization loadBulletTexture $ \texture -> liftSem $
  forEachBullet (renderBullet texture)
  where
    renderBullet :: Member Graphics r => Texture Any -> BulletType -> V2 Double -> Sem r ()
    renderBullet texture (Straight) pos =
          render $ makeRenderInstruction
                    5
                    texture
                    Nothing
                    (Just $ Placed (round <$> pos - V2 6 6) $ Rectangle 12 12)
                    0
                    Nothing
                    (V2 False False)

loadBulletTexture :: Member Graphics r => Sem r (Texture Any)
loadBulletTexture = makeTexture $ do
  texture <- createStaticTexture $ Rectangle 12 12
  let img = makeArray Seq (Sz (Ix2 12 12)) $ \(Ix2 x y) -> let v = toEnum (abs (x - y)) in (v,v,v,255)
  updateStaticTexture texture Nothing img
  pure $ toAnyTexture texture

renderPlayer :: P.Members [(ApecsSystem World), Embed IO, Graphics] r => Signal (Sem r) ()
renderPlayer = withInitialization loadPlayerTexture $ \texture -> readerSignal getPlayerPosition . feedback (0 :: Int) . liftSem $ do
  playerPos <- ask @(V2 Double)
  frameCount <- get @Int
  let renderInstruction shipPosOnTexture = makeRenderInstruction
          10
          texture
          shipPosOnTexture
          (Just $ Placed (round <$> playerPos - V2 16 36) $ Rectangle 36 72)
          0
          Nothing
          (V2 False False)
  render . renderInstruction $ chooseImage frameCount
  put (succ frameCount)
  when (frameCount == 20) $ put (0 :: Int)
  where ship1 = (Just $ Placed (V2 0 0) $ Rectangle 36 72)
        ship2 = (Just $ Placed (V2 36 0) $ Rectangle 36 72)
        ship3 = (Just $ Placed (V2 72 0) $ Rectangle 36 72)
        chooseImage frameCount
          | frameCount < 6 = ship1
          | frameCount < 11 = ship2
          | frameCount < 16 = ship1
          | frameCount < 21 = ship3
          | otherwise = ship1

loadPlayerTexture :: Member Graphics r => Sem r (Texture Any)
loadPlayerTexture = makeTexture $ do
  img1 <- loadImage "media/ship1.bmp"
  img2 <- loadImage "media/ship2.bmp"
  img3 <- pure $ flipImage (V2 True False) <$> img2
  let (mergedImage :: Image) = either
        (const emptyImage)
        id
        (fmap (computeAs S) $ join $ appendM (Dim 2) <$> (fmap (computeAs S) . join $ appendM (Dim 2) <$> img1 <*> img2) <*> img3)
  staticTexture <- createStaticTexture (Rectangle 108 72)
  updateStaticTexture staticTexture Nothing mergedImage
  pure $ toAnyTexture staticTexture
    where emptyImage = M.replicate Seq (Sz (Ix2 108 72)) $ (255,255,255,255)
