{-# LANGUAGE BangPatterns #-}

module Render where

import Effect.Apecs
import Apecs.Physics hiding (ask, get, asks)
import Control.Monad.IO.Class
import Polysemy as P
import Polysemy.Reader
import Polysemy.Input
import Sigma.Signal
import Debug.Trace
import Polysemy.State
import Data.Function ((&))
import Control.Lens
import qualified Data.Vector.Storable as V
import Data.Text (Text, pack)
import Debug.Trace

import Control.Monad
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Graphics.GPipe
import GPipe.Image
import Debug.Trace
import GPipe.Interface
import Data.Word
import Types
import Player
import Bullets
import Enemies

data RenderData = RenderData
  { playerShader :: (V2 Int -> MyWindowAction ())
  , argoShader :: (V2 Int -> MyWindowAction ())
  }

initializeRenderData :: P.Member (MyWindowIO) r => Sem r RenderData
initializeRenderData = executeWindowAction $ RenderData <$> makePlayerShader <*> makeArgoShader


renderWorld :: P.Members [Reader RenderData, ApecsSystem World, Embed IO, MyWindowIO] r => Signal (Sem r) ()
renderWorld = liftAction $ do
  liftIO $ print "rendering"
  executeWindowAction clearBuffer
  player
  enemies
  executeWindowAction swapBuffers

player :: P.Members [Reader RenderData, (ApecsSystem World), Embed IO, MyWindowIO] r => Sem r ()
player = do
  pos <- executeApecsSystem @World getPlayerPosition
  runShader <- asks playerShader
  executeWindowAction$ do
    runShader (round <$> pos - V2 18 36)

enemies :: P.Members [Reader RenderData, (ApecsSystem World), Embed IO, MyWindowIO] r => Sem r ()
enemies = liftIO (print "test enemies") *> forEachEnemy renderEnemy
  where
    renderEnemy :: P.Members [Embed IO, Reader RenderData, MyWindowIO] r => EnemyType -> V2 Double -> Sem r ()
    renderEnemy enemyType position = case enemyType of
      (Argo _) -> do
        liftIO $ print "Test"
        shadeArgo <- asks argoShader
        executeWindowAction $ shadeArgo (round <$> position - V2 20 20)
      _ -> liftIO $ print "Something went wrong"

swapBuffers :: MyWindowAction ()
swapBuffers = makeWindowAction $ \win -> swapWindowBuffers win

clearBuffer :: MyWindowAction ()
clearBuffer = makeWindowAction $ \win -> render $ clearWindowColor win 0.5

adjustViewPort :: V2 Int -> ViewPort -> ViewPort
adjustViewPort (V2 winX winY) (ViewPort (V2 posX posY) (V2 logicalSizeX logicalSizeY)) =
    let sizeY = winY * logicalSizeY `quot` 480
        sizeX = winY * logicalSizeX `quot` 480
        newPosY = winY * posY `quot` 480
        newPosX = ((winX - winY) `quot` 2) + winY * posX `quot` 480
    in (ViewPort (V2 newPosX newPosY) (V2 sizeX sizeY))


makePlayerShader :: MyWindowAction (V2 Int -> MyWindowAction ())
makePlayerShader = makeWindowAction $ \win -> do
  let vertices = [V2 (-1) (-1), V2 1 (-1), V2 (-1) 1, V2 1 1]
  vertexBuffer :: Buffer (B2 Float) <- newBuffer 4
  writeBuffer vertexBuffer 0 vertices
  tex <- newTexture2D RGBA8 (V2 36 72) 1
  Image _ imgData <- liftIO $ readImage2 "/home/simon/Projects/glGame/media/ship1.bmp"
  writeTexture2D tex 0 (V2 0 0) (V2 36 72) (reverse $ V.toList imgData)
  shader <- compileShader $ do
    primitiveStream <- toPrimitiveStream fst
    let primitiveStream2 = fmap (\(V2 x y) -> (V4 x y 0 1, ((V2 x y + V2 1 1) / V2 2 2))) primitiveStream
    fragmentStream <- rasterize (\(_,(pos, winSize)) -> (Front, adjustViewPort winSize $ ViewPort pos ((V2 36 72)), DepthRange 0 1)) primitiveStream2
    let filter = SamplerNearest
        edge = (pure ClampToEdge, undefined)
    samp <- newSampler2D (const (tex, filter, edge))
    let sampleTexture = sample2D samp SampleAuto Nothing Nothing
        fragmentStream2 = fmap sampleTexture fragmentStream

    drawWindowColor (const (win, ContextColorOption blending (pure True))) (fragmentStream2)
  pure $ \pos -> makeWindowAction $ \_ -> do
    winSize <- getFrameBufferSize win
    render $ do
      vertexArray <- newVertexArray vertexBuffer
      primitiveArray <- pure $ toPrimitiveArray TriangleStrip vertexArray
      shader (primitiveArray, (pos, winSize))
  where blending = BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors SrcAlpha OneMinusSrcAlpha) (V4 (-1) (-1) (-1) 1)

makeArgoShader :: MyWindowAction (V2 Int -> MyWindowAction ())
makeArgoShader = makeWindowAction $ \win -> do
  let vertices = [V2 (-1) (-1), V2 1 (-1), V2 (-1) 1, V2 1 1]
  vertexBuffer :: Buffer (B2 Float) <- newBuffer 4
  writeBuffer vertexBuffer 0 vertices
  shader <- compileShader $ do
    primitiveStream <- fmap (\(V2 x y) -> (V4 x y 0 1, V2 x y)) <$> toPrimitiveStream fst
    fragmentStream <- rasterize (\(_,(pos, winSize)) -> (Front, adjustViewPort winSize $ ViewPort pos ((V2 40 40)), DepthRange 0 1)) primitiveStream
    drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) $
      (\(V2 x y) -> V4 x y 0 1) <$> fragmentStream
  pure $ \pos -> makeWindowAction $ \_ -> do
    winSize <- getFrameBufferSize win
    liftIO $ print pos
    render $ do
      vertexArray <- newVertexArray vertexBuffer
      primitiveArray <- pure $ toPrimitiveArray TriangleStrip vertexArray
      shader (primitiveArray, (pos, winSize))
--  where blending = BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors SrcAlpha OneMinusSrcAlpha) (V4 (-1) (-1) (-1) 1)
