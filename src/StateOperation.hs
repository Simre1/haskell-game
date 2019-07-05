module StateOperation
  ( StateOperation (..)
  , soSet
  , soGet
  , mapStateOperation
  , stateVarToStateOperation
  ) where

import Data.StateVar

data StateOperation a x where
  SOSet :: a -> StateOperation a ()
  SOGet :: (a -> c) -> StateOperation a c

soSet :: a -> (StateOperation a () -> m ()) -> m ()
soSet a f = f (SOSet a)

soGet :: (StateOperation a a -> m a) -> m a
soGet f = f (SOGet id)

mapStateOperation :: (a -> b) -> (b -> a) -> StateOperation a x -> StateOperation b x
mapStateOperation map contramap (SOSet a) = SOSet (map a)
mapStateOperation map contramap (SOGet f) = SOGet (f . contramap)

stateVarToStateOperation :: StateVar a -> StateOperation a x -> IO x
stateVarToStateOperation stateVar = \case
  (SOSet a) -> stateVar $= a
  (SOGet f) -> f <$> get stateVar
