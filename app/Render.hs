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

import Shapes2D
import Data.Massiv.Array as M hiding ((!))
import Control.Exception
import Control.Monad

import World
import Player

renderWorld :: P.Members [(ApecsSystem World), Embed IO, Graphics] r => Signal (Sem r) ()
renderWorld = withInitialization loadPlayerTexture $ \texture -> readerSignal getPlayerPosition . feedback (0 :: Int) . liftSem $ do
  playerPos <- ask @(V2 Double)
  frameCount <- get @Int
  let renderInstruction shipPosOnTexture = makeRenderInstruction
          0
          texture
          shipPosOnTexture
          (Just $ Placed (round <$> playerPos - V2 16 36) $ Rectangle 36 72)
          0
          Nothing
          (V2 False False)
  render . renderInstruction $ if (frameCount < 6)
    then ship1
    else ship2
  put (succ frameCount)
  when (frameCount > 10) $ put (0 :: Int)
  where ship1 = (Just $ Placed (V2 0 0) $ Rectangle 36 72)
        ship2 = (Just $ Placed (V2 36 0) $ Rectangle 36 72)

loadPlayerTexture :: Member Graphics r=> Sem r (Texture Any)
loadPlayerTexture = makeTexture $ do
  img1 <- loadImage "media/ship1.bmp"
  img2 <- loadImage "media/ship2.bmp"
  let (mergedImage :: Image) = either
        (const emptyImage)
        id
        (fmap (computeAs S) $ join $ appendM (Dim 2) <$> img1 <*> img2)
  staticTexture <- traceShow (size mergedImage) $ createStaticTexture (Rectangle 72 72)
  updateStaticTexture staticTexture Nothing mergedImage
  pure $ toAnyTexture staticTexture
    where emptyImage = M.replicate Seq (Sz (Ix2 72 72)) $ (255,255,255,255)
