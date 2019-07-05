{-# LANGUAGE UndecidableInstances #-}
module Effect.Store (Store, runStore, sGet, sMod, maybeSMod) where

import Import
import Data.Typeable
import Data.Default
import Data.IORef
import Unsafe.Coerce
import Data.Map
import Data.Coerce

import Control.Effect.Reader
import Sigma

sGet :: (Member (Store) sig, Carrier sig m, Typeable a, Default a) => Signal m x a
sGet = Signal . const $ do
  get <- createGetter
  a <- get
  return (a, arrM . const $ get)

sMod :: (Member (Store) sig, Carrier sig m, Typeable a, Default a) => Signal m (a -> a) ()
sMod = Signal $ \a -> do
  mod <- createModifier
  mod a
  return ((), arrM mod)

maybeSMod :: (Member (Store) sig, Carrier sig m, Typeable a, Default a) => Signal m (Maybe (a -> a)) ()
maybeSMod = Signal $ \maybeA -> do
   mod <- createModifier
   maybeMod mod maybeA
   return (() , arrM $ maybeMod mod)
   where maybeMod mod' val = maybe (pure ()) mod' val

runStore :: forall m a b. (Functor m, MonadIO m) => Signal (StoreC m) a b -> Signal m a b
runStore signal = Signal $ \a -> do
  storeRef <- initStoreRef
  let newSignal = signalSimpleMorph (runStoreC storeRef) $ updateSignal signal
  (b, cont) <- stepSignal newSignal a
  return (b, cont)
  where updateSignal signal' = Signal $ \a' -> do
          (b',cont') <- stepSignal signal' a'
          updateStore
          return (b',updateSignal cont')
  

newtype StoreAction a = StoreAction {runStoreAction :: IO a} deriving (Functor, Applicative, Monad)


data Store (m2 :: * -> *) k where
  CreateModifier :: (Typeable a, Default a) => (((a -> a) -> StoreAction ()) -> k) -> Store m2 k
  CreateGetter :: (Typeable a, Default a) => (StoreAction a -> k) -> Store m2 k
  RunStoreAction :: (a -> k) -> StoreAction a -> Store m2 k

instance Functor (Store m2) where
  fmap f (CreateModifier a) = CreateModifier ((f .)  a)
  fmap f (CreateGetter a) = CreateGetter ((f .)  a)

instance HFunctor (Store) where
 hmap _ = coerce

instance Effect (Store) where
  handle state handler = coerce . fmap (handler . (<$ state))

newtype StoreC m a = StoreC (ReaderC StoreRef m a) deriving (Monad, Applicative, Functor, MonadIO)

newtype StoreRef = StoreRef (IORef (GameStore, IO ()))

initStoreRef :: MonadIO m => m StoreRef
initStoreRef = StoreRef <$> liftIO (newIORef (GameStore mempty, mempty))

runStoreC :: StoreRef -> StoreC m a -> m a
runStoreC ref (StoreC readerC) = runReaderC readerC ref

updateStore :: MonadIO m => StoreC m ()
updateStore = StoreC . ReaderC $ \(StoreRef ref) -> liftIO $ join (snd <$> readIORef ref) >> modifyIORef' ref (\(a,_) -> (a,mempty))

instance (Carrier sig m, Effect sig, MonadIO m, Monad m) => Carrier (Store :+: sig) (StoreC m) where
  eff (L (CreateModifier bindModifier)) = bindModifier =<<
    (StoreC . ReaderC $ \(StoreRef ref) -> liftIO $ do
        (gs, c) <- readIORef ref
        (newGs, f) <- modifyStateIO gs
        writeIORef ref (newGs, c)
        return $ \a -> StoreAction $ modifyIORef ref (\(gs, c) -> (gs,c >> f a)))
  eff (L (CreateGetter bindGetter)) = bindGetter =<<
    (StoreC . ReaderC $ \(StoreRef ref) -> liftIO $ do
          (gs, c) <- readIORef ref
          (newGs, a) <- getStateIO gs
          writeIORef ref (newGs,c)
          return $ StoreAction a)
  eff (L (RunStoreAction k sa)) = liftIO (runStoreAction sa) >>= k
  eff (R other) = StoreC $ eff $ R $ handleCoercible other



createGetter :: (Member Store sig, Carrier sig m, Member Store sig2, Carrier sig2 m2) => forall a. (Typeable a, Default a) => m (m2 a)
createGetter = send $ CreateGetter $ send . RunStoreAction (pure . pure)


createModifier :: (Member (Store) sig, Carrier sig m, Member Store sig2, Carrier sig2 m2) => forall a. (Typeable a, Default a) => m ((a -> a) -> m2 ())
createModifier = send $ CreateModifier $ pure . run
  where
    run :: (Member Store sig3, Carrier sig3 m3) => ((a -> a) -> StoreAction ()) -> ((a -> a) -> m3 ())
    run f = send . RunStoreAction pure . f

data StateWrapperIO = forall s. (Typeable s) => StateWrapperIO (IORef s)

data GameStore = GameStore {statesIO :: Map TypeRep StateWrapperIO}

getStateIO :: forall a. (Typeable a, Default a) => GameStore -> IO (GameStore, IO a)
getStateIO gs = maybe
  (do
      ref <- newIORef $ (def :: a)
      return (GameStore $ insert (typeRep (Proxy :: Proxy a)) (StateWrapperIO ref) (statesIO gs), readIORef ref)
  )
  (\ref -> return (gs, readIORef ref))
  ((\(StateWrapperIO a) -> unsafeCoerce a) <$> lookup (typeRep (Proxy :: Proxy a)) (statesIO gs))

modifyStateIO :: forall a. (Typeable a, Default a) => GameStore -> IO (GameStore, ((a -> a) -> IO ()))
modifyStateIO gs = do
  maybe
    (do
       ref <- newIORef $ (def :: a)
       return (GameStore $ insert (typeRep (Proxy :: Proxy a)) (StateWrapperIO ref) (statesIO gs), modifyIORef' ref)
    )
    (\ref -> return (gs, modifyIORef' ref))
    ((\(StateWrapperIO x) -> unsafeCoerce x) <$> lookup (typeRep (Proxy :: Proxy a)) (statesIO gs))


