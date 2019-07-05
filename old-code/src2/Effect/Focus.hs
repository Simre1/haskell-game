{-# LANGUAGE TemplateHaskell #-}
module Effect.Focus
  ( Focus
  , runFocus
  , FocusBox (..)
  , getFocusBox
  , modifyFocusBox
  , setFocusBox
  , inFocus
  )
  where

import Import
import Shapes2D
import Data.Coerce
import Data.IORef
import Sigma

newtype FocusBox a = FocusBox {focusBoxRectangle :: Rectangle a} deriving Functor


instance HasBounds FocusBox where
  getBounds = getBounds . focusBoxRectangle


data Focus (m :: * -> *) k where
  GetFocusBox :: Focus m (FocusBox Int)
  ModifyFocusBox :: (FocusBox Int -> FocusBox Int) -> Focus m ()

instance HFunctor Focus where
  hmap _ = coerce

instance Effect Focus where
  handle state handler = coerce . fmap (handler . (<$ state))

newtype FocusC m a = FocusC (ReaderC (IORef (FocusBox Int)) m a) deriving (Functor, Applicative, Monad, MonadIO)

runFocusC :: IORef (FocusBox Int) -> FocusC m a -> m a
runFocusC ref (FocusC reader) = runReader ref reader

runFocus :: MonadIO m => FocusBox Int -> Signal (FocusC m) a b -> Signal m a b
runFocus box signal = Signal $ \a -> do
  ref <- liftIO $ newIORef box
  stepSignal (signalSimpleMorph (runFocusC ref) signal) a

instance (Effect sig, Carrier sig m, MonadIO m) => Carrier (Focus :+: sig) (FocusC m) where
  eff (L (GetFocusBox k)) = FocusC ask >>= liftIO . readIORef >>= k
  eff (L (ModifyFocusBox f k)) = FocusC ask >>= liftIO . (flip modifyIORef) f >> k
  eff (R other) = FocusC $ eff $ R $ handleCoercible other

getFocusBox :: (Member Focus sig, Carrier sig m) => m (FocusBox Int)
getFocusBox = send $ GetFocusBox pure

modifyFocusBox :: (Member Focus sig, Carrier sig m) => (FocusBox Int -> FocusBox Int) -> m ()
modifyFocusBox f = send $ ModifyFocusBox f (pure ())

setFocusBox :: (Member Focus sig, Carrier sig m) => FocusBox Int -> m ()
setFocusBox = modifyFocusBox . const

inFocus :: (Member Focus sig, Carrier sig m) => BoundingBox Int -> m Bool
inFocus bb = boundingBoxCheckIntersection bb . getBounds <$> getFocusBox






