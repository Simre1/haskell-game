module Effect.Graphics.RenderActions
  ( rectangle
  , fullScreen
  , createRenderAction
  )
  where

import Import
import qualified SDL
import Effect.Graphics.Internal
import Sigma
import Shapes2D


fullScreen :: RenderAction ()
fullScreen = createRenderAction $ \r _ -> SDL.fillRect r Nothing

rectangle :: V2 Int -> Rectangle Int -> RenderAction ()
rectangle pos rect = createRenderAction $ \r c -> do
  let sdlRect =
        let newPos = adjustPointForRender c pos
            newDim = adjustDistanceForRender . rectangleDimensions $ rect
        in SDL.Rectangle (SDL.P $ toEnum <$> newPos) (toEnum <$> newDim)
  SDL.fillRect r $ pure sdlRect
  pure ()

--
-- circle :: V2 Int -> Circle Int -> RenderAction ()
-- circle center circle = (\newCenter -> rectangle $ Just $ newRectangle (V2 50 50)) =<<


-- | Only use this if none of the given RenderActions suffice. Be especially mindful of setting renderer options since this will also affect *all OTHER RenderActions*!
--createRenderAction :: (SDL.Renderer -> IO a) -> RenderAction a
createRenderAction = RenderAction
