module Effect.Graphics.ResourceActions
  ( createGraphicsResource
  ) where

import Import
import Effect.Graphics.Internal
import qualified SDL


-- | Only use this if none of the given ResourceActions suffice.
createGraphicsResource :: (SDL.Renderer -> IO a) -> GraphicsResource a
createGraphicsResource = GraphicsResource

