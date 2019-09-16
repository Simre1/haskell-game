{-# LANGUAGE ScopedTypeVariables #-}

module Render.Level.Player where

import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)

import Graphics.ReadImage (Image(..), readImageRGBA8)
import Window.GPipe

import Graphics.Shader.ViewPort (adjustViewPort)
import MyWindow (MyWindowAction)


makePlayerShader :: MyWindowAction (V2 Int -> MyWindowAction ())
makePlayerShader = makeWindowAction $ \win -> do
  let vertices = [V2 (-1) (-1), V2 1 (-1), V2 (-1) 1, V2 1 1]
  vertexBuffer :: Buffer (B2 Float) <- newBuffer 4
  writeBuffer vertexBuffer 0 vertices
  tex <- newTexture2D RGBA8 (V2 36 72) 1
  img <- liftIO $ readImageRGBA8 $ pack "./media/ship1.png"
  writeTexture2D tex 0 (V2 0 0) (V2 36 72) img
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
      shader (primitiveArray, (pos - V2 18 36, winSize))
  where blending = BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors SrcAlpha OneMinusSrcAlpha) (V4 (-1) (-1) (-1) 1)
