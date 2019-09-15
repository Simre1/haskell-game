{-# LANGUAGE ScopedTypeVariables #-}

module Render.Level.Background where

import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)

import Graphics.ReadImage (Image(..), readImageGrayscale)
import Window.GPipe

import Graphics.Shader.ViewPort (adjustViewPort)
import MyWindow (MyWindowAction)

makeBackgroundShader :: MyWindowAction (Float -> MyWindowAction ())
makeBackgroundShader = makeWindowAction $ \win -> do
  let vertices = [V2 (-1) (-1), V2 1 (-1), V2 (-1) 1, V2 1 1]

  progressBuffer :: Buffer (Uniform (B Float)) <- newBuffer 1

  tex <- newTexture2D R8 (V2 480 480) 1
  img <- liftIO $ readImageGrayscale $ pack "./media/noise_gray.png"
  writeTexture2D tex 0 (V2 0 0) (V2 480 480) img

  vertexBuffer :: Buffer (B2 Float) <- newBuffer 4
  writeBuffer vertexBuffer 0 vertices
  shader <- compileShader $ do
    primitiveStream <- fmap (\(V2 x y) -> (V4 x y 0 1, V2 x y)) <$> toPrimitiveStream fst
    fragmentStream <- rasterize (\(_,winSize) -> (Front, adjustViewPort winSize $ ViewPort (V2 0 0) ((V2 480 480)), DepthRange 0 1)) primitiveStream
    progress <- getUniform (const (progressBuffer,0))
    let filter = SamplerFilter Nearest Nearest Nearest Nothing
        edge = (pure ClampToEdge, undefined)
    samp <- newSampler2D (const (tex, filter, edge))
    let sampleTexture (V2 x y) = (V2 x y,sample2D samp SampleAuto Nothing Nothing (V2 x (fract' $ y+progress)))
    drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) $
      fragShader progress . sampleTexture . (\(V2 x y) -> (floor' <$> V2 x y * V2 480 480) / V2 480 480) <$> fragmentStream
  pure $ \progress -> makeWindowAction $ \_ -> do
    winSize <- getFrameBufferSize win
    writeBuffer progressBuffer 0 [progress/480]

    render $ do
      vertexArray <- newVertexArray vertexBuffer
      primitiveArray <- pure $ toPrimitiveArray TriangleStrip vertexArray
      shader (primitiveArray, winSize)
  where
    fragShader :: FFloat -> (V2 FFloat, FFloat) -> V4 FFloat
    fragShader progress (V2 x y, noise) =
          background + clouds + clouds2
          where
            background =
              let
                r = 0.004
                g = 0.003
                b = 0.007 * (3 + sin (x + progress + y))
              in V4 r g b 1
            clouds =
              let lumen = noise * (1 + sin ((y + progress) * 2) - sin (x + (y+progress)))
              in V4 (0.02 * lumen) (0.002*lumen) (0.015 * lumen) 1
            clouds2 =
              let lumen = 0.04 * noise * noise * sqrt ((clamp (sin (y + progress - x) + sin (4*(y+progress+(abs $ x-x^2))) - noise) 0 10) ^ 2 ^ 2)
              in V4 (lumen * 1.5) lumen (lumen * 2) 1
