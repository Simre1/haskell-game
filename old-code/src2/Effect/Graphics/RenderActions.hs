module Effect.Graphics.RenderActions
  ( rectangle
  , createRenderAction
  , StateVarAction
  , svGet
  , svPut
  , rendererDrawColor
  )
  where

import Import
import qualified SDL
import Effect.Graphics.Internal
import Sigma
import Shapes2D


data StateVarAction a b where
  SVGet :: StateVarAction a a
  SVPut :: a -> StateVarAction a ()

svGet = SVGet
svPut = SVPut

renderActionFromStateVar :: (SDL.Renderer -> StateVar a) -> StateVarAction a b -> RenderAction b
renderActionFromStateVar sva (SVPut a) = createRenderAction $ \r _ -> sva r $= a
renderActionFromStateVar sva SVGet = createRenderAction $ \r _ -> get (sva r)

rendererDrawColor :: StateVarAction (AlphaColour Double) b -> RenderAction b
rendererDrawColor = renderActionFromStateVar stateVar
  where stateVar r = mapStateVar alphaColourToV4 v4ToAlphaColour $ SDL.rendererDrawColor r

rectangle :: Maybe (Rectangle Int) -> RenderAction ()
rectangle rect = createRenderAction $ \r c -> do
  let sdlRect = do
        pos <- adjustYPosition c . adjustToCameraPosition c . rectangleAnchor <$> rect
        dim <- inverseY . rectangleDimensions <$> rect
        pure $ SDL.Rectangle (SDL.P $ toEnum <$> pos) (toEnum <$> dim)
  SDL.fillRect r sdlRect
  pure ()


-- | Only use this if none of the given RenderActions suffice. Be especially mindful of setting renderer options since this will also affect *all OTHER RenderActions*!
--createRenderAction :: (SDL.Renderer -> IO a) -> RenderAction a
createRenderAction = RenderAction

