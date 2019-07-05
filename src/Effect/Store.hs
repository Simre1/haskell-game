{-# LANGUAGE TemplateHaskell #-}
module Effect.Store (runStore, Store, sGet, sMod, maybeSMod) where

import Data.Typeable
import Data.Default
import Data.IORef
import Unsafe.Coerce
import Data.Map as M
import Data.Coerce
import Polysemy
import Control.Monad.IO.Class
import Control.Monad
import Control.Arrow

import Sigma


newtype StoreAction a = StoreAction {unwrapStoreAction :: IO a} deriving (Functor, Applicative, Monad)


data Store (m2 :: * -> *) k where
  CreateModifier :: (Typeable a, Default a) => Store m2 ((a -> a) -> StoreAction ())
  CreateGetter :: (Typeable a, Default a) => Store m2 (StoreAction a)
  RunStoreAction :: StoreAction a -> Store m2 a

makeSem ''Store


runStoreE :: Member (Lift IO) r => StoreRef -> Sem (Store : r) a -> Sem r a
runStoreE (StoreRef ref) = interpret $ \case
  (CreateModifier) -> liftIO $ do
    (gs,c) <- readIORef ref
    (newGs, f) <- modifyStateIO gs
    writeIORef ref (newGs, c)
    pure $ \a -> StoreAction $ modifyIORef ref (second (>> f a))
  (CreateGetter) -> liftIO $ do
    (gs, c) <- readIORef ref
    (newGs, a) <- getStateIO gs
    writeIORef ref (newGs,c)
    pure $ StoreAction a
  (RunStoreAction action) -> liftIO $ unwrapStoreAction action


sGet :: (Member (Store) r, Typeable a, Default a) => Signal (Sem r) x a
sGet = Signal . const $ do
  get <- runStoreAction <$> createGetter
  a <- get
  return (a, arrM . const $ get)

sMod :: (Member (Store) r, Typeable a, Default a) => Signal (Sem r) (a -> a) ()
sMod = Signal $ \a -> do
  mod <- (runStoreAction.) <$> createModifier
  mod a
  return ((), arrM mod)

maybeSMod :: (Member (Store) r, Typeable a, Default a) => Signal (Sem r) (Maybe (a -> a)) ()
maybeSMod = Signal $ \maybeA -> do
   mod <- (runStoreAction.) <$> createModifier
   maybeMod mod maybeA
   return (() , arrM $ maybeMod mod)
   where maybeMod mod' val = maybe (pure ()) mod' val

runStore :: Member (Lift IO) r => Signal (Sem (Store : r)) a b -> Signal (Sem r) a b
runStore signal = Signal $ \a -> do
  storeRef <- initStoreRef
  let newSignal = signalSimpleMorph (runStoreE storeRef) $ updateSignal storeRef signal
  (b, cont) <- stepSignal newSignal a
  return (b, cont)
  where updateSignal storeRef signal' = Signal $ \a' -> do
          (b',cont') <- stepSignal signal' a'
          updateStore storeRef
          return (b',updateSignal storeRef cont')

updateStore :: MonadIO m => StoreRef -> m ()
updateStore (StoreRef ref) = liftIO $ join (snd <$> readIORef ref) >> modifyIORef' ref (\(a,_) -> (a,mempty))



newtype StoreRef = StoreRef (IORef (GameStore, IO ()))

initStoreRef :: MonadIO m => m StoreRef
initStoreRef = StoreRef <$> liftIO (newIORef (GameStore mempty, mempty))

data StateWrapperIO = forall s. (Typeable s) => StateWrapperIO (IORef s)

data GameStore = GameStore {statesIO :: Map TypeRep StateWrapperIO}

getStateIO :: forall a. (Typeable a, Default a) => GameStore -> IO (GameStore, IO a)
getStateIO gs = maybe
  (do
      ref <- newIORef $ (def :: a)
      return (GameStore $ insert (typeRep (Proxy :: Proxy a)) (StateWrapperIO ref) (statesIO gs), readIORef ref)
  )
  (\ref -> return (gs, readIORef ref))
  ((\(StateWrapperIO a) -> unsafeCoerce a) <$> M.lookup (typeRep (Proxy :: Proxy a)) (statesIO gs))

modifyStateIO :: forall a. (Typeable a, Default a) => GameStore -> IO (GameStore, ((a -> a) -> IO ()))
modifyStateIO gs = do
  maybe
    (do
       ref <- newIORef $ (def :: a)
       return (GameStore $ insert (typeRep (Proxy :: Proxy a)) (StateWrapperIO ref) (statesIO gs), modifyIORef' ref)
    )
    (\ref -> return (gs, modifyIORef' ref))
    ((\(StateWrapperIO x) -> unsafeCoerce x) <$> M.lookup (typeRep (Proxy :: Proxy a)) (statesIO gs))
