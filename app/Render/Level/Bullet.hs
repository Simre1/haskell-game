{-# LANGUAGE ScopedTypeVariables #-}

module Render.Level.Bullet where

import Window.GPipe

import Graphics.Shader.ViewPort (adjustViewPort)
import MyWindow (MyWindowAction)


makeStraightBulletShader :: MyWindowAction (V2 Int -> MyWindowAction ())
makeStraightBulletShader = makeWindowAction $ \win -> do
  let vertices = [V2 (-1) (-1), V2 1 (-1), V2 (-1) 1, V2 1 1]
  vertexBuffer :: Buffer (B2 Float) <- newBuffer 4
  writeBuffer vertexBuffer 0 vertices
  shader <- compileShader $ do
    primitiveStream <- fmap (\(V2 x y) -> (V4 x y 0 1, 0 :: VFloat)) <$> toPrimitiveStream fst
    fragmentStream <- rasterize (\(_,(pos, winSize)) -> (Front, adjustViewPort winSize $ ViewPort pos ((V2 12 12)), DepthRange 0 1)) primitiveStream
    drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) $
      (\_ -> V4 1 1 1 1 :: V4 FFloat) <$> fragmentStream
  pure $ \pos -> makeWindowAction $ \_ -> do
    winSize <- getFrameBufferSize win
    render $ do
      vertexArray <- newVertexArray vertexBuffer
      primitiveArray <- pure $ toPrimitiveArray TriangleStrip vertexArray
      shader (primitiveArray, (pos - V2 6 6, winSize))
