{-# LANGUAGE ScopedTypeVariables #-}

module Render.Level.Enemy where

import Window.GPipe

import Graphics.Shader.ViewPort (adjustViewPort)
import MyWindow (MyWindowAction)


makeArgoShader :: MyWindowAction (V2 Int -> MyWindowAction ())
makeArgoShader = makeWindowAction $ \win -> do
  let vertices = [V2 (-1) (-1), V2 1 (-1), V2 (-1) 1, V2 1 1]
  vertexBuffer :: Buffer (B2 Float) <- newBuffer 4
  writeBuffer vertexBuffer 0 vertices
  shader <- compileShader $ do
    primitiveStream <- fmap (\(V2 x y) -> (V4 x y 0 1, V2 x y)) <$> toPrimitiveStream fst
    fragmentStream <- rasterize (\(_,(pos, winSize)) -> (Front, adjustViewPort winSize $ ViewPort pos ((V2 40 40)), DepthRange 0 1)) primitiveStream
    drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) $
      (\(V2 x y) -> V4 x y 0 1) . (\(V2 x y) -> (floor' <$> V2 x y * V2 40 40) / V2 40 40) <$> fragmentStream
  pure $ \pos -> makeWindowAction $ \_ -> do
    winSize <- getFrameBufferSize win
    render $ do
      vertexArray <- newVertexArray vertexBuffer
      primitiveArray <- pure $ toPrimitiveArray TriangleStrip vertexArray
      shader (primitiveArray, (pos - V2 20 20, winSize))
