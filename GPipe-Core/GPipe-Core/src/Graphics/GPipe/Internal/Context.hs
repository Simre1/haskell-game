{-# LANGUAGE TypeFamilies, RankNTypes, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, GADTs, DeriveDataTypeable #-}

module Graphics.GPipe.Internal.Context
(
    ContextHandler(..),
    ContextT(),
    GPipeException(..),
    runContextT,
    newWindow,
    deleteWindow,
    swapWindowBuffers,
    getFrameBufferSize,
    withContextWindow,
    WindowState(..),
    RenderState(..),
    liftNonWinContextIO,
    liftNonWinContextAsyncIO,
    addContextFinalizer,
    Window(..),
    addVAOBufferFinalizer,
    addFBOTextureFinalizer,
    getVAO, setVAO,
    getFBO, setFBO,
    ContextData,
    VAOKey(..), FBOKey(..), FBOKeys(..),
    Render(..), render,
    registerRenderWriteTexture,
    getLastRenderWin,
    asSync
)
where

import Graphics.GPipe.Internal.Format
import Control.Monad.Exception (MonadException, Exception, MonadAsyncException,bracket)
import Control.Monad.Trans.Reader
import qualified Control.Monad.Fail as MF
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Applicative (Applicative, (<$>))
import Data.Typeable
import qualified Data.IntSet as Set
import qualified Data.IntMap.Strict as IMap
import Data.IntMap ((!))
import qualified Data.Map.Strict as Map
import Graphics.GL.Core33
import Graphics.GL.Types
import Control.Concurrent.MVar
import Data.IORef
import Control.Monad
import Data.List (delete)
import Foreign.C.Types
import Data.Maybe
import Linear.V2 (V2(V2))
import Control.Monad.Trans.Except
import Control.Exception (throwIO)
import Control.Arrow
import Control.Monad.Trans.State.Strict

-- | Class implementing a window handler that can create openGL contexts, such as GLFW or GLUT
class ContextHandler ctx where
  -- | Implementation specific context handler parameters, eg error handling and event processing policies
  data ContextHandlerParameters ctx
  -- | Implementation specific window type
  type ContextWindow ctx
  -- | Implementation specific window parameters, eg initial size and border decoration
  type WindowParameters ctx
  -- | Create a context handler. Called from the main thread
  contextHandlerCreate :: ContextHandlerParameters ctx -> IO ctx
  -- | Delete the context handler. All contexts created from this handler will be deleted using contextDelete prior to calling this.
  contextHandlerDelete :: ctx -> IO ()
  -- | Create a new context sharing all other contexts created by this ContextHandler. If the parameter is Nothing,
  --   a hidden off-screen context is created, otherwise creates a window with the provided window bits and implementation specific parameters.
  --   Only ever called from the mainthread (i.e. the thread that called contextHandlerCreate).
  createContext :: ctx -> Maybe (WindowBits, WindowParameters ctx) -> IO (ContextWindow ctx)
  -- | Run an OpenGL IO action in this context, that doesn't return any value to the caller. This may be run after contextDelete or contextHandlerDelete has been called.
  --   The thread calling this may not be the same creating the context (for finalizers it is most definetly not).
  --   May also be called on previously deleted windows in the case of finalizers.
  contextDoAsync :: ctx -> Maybe (ContextWindow ctx) -> IO () -> IO ()
  -- | Swap the front and back buffers in the context's default frame buffer.
  --   Only ever called from the mainthread (i.e. the thread that called 'contextHandlerCreate').
  --   Never called on deleted windows.
  contextSwap :: ctx -> ContextWindow ctx -> IO ()
  -- | Get the current size of the context's default framebuffer (which may change if the window is resized).
  --   Only ever called from the mainthread (i.e. the thread that called 'contextHandlerCreate')
  contextFrameBufferSize :: ctx -> ContextWindow ctx -> IO (Int, Int)
  -- | Delete a context and close any associated window.
  --   Only ever called from the mainthread (i.e. the thread that called 'contextHandlerCreate'). Only ever called once per window,
  --   and will always be called for each window before the context is deleted with 'contextHandlerDelete'.
  contextDelete :: ctx -> ContextWindow ctx -> IO ()


-- | The monad transformer that encapsulates a GPipe context (which wraps an OpenGl context).
--
--   A value of type @ContextT ctx m a@ is an action on a context with these parameters:
--
--   [@ctx@] The context handler.
--
--   [@os@] An abstract type that is used to denote the object space. This is an forall type defined by the 'runContextT' call which will restrict any objects created inside this context
--          to be returned from it or used by another context (the same trick as the 'ST' monad uses).
--
--   [@m@] The monad this monad transformer wraps. Need to have 'IO' in the bottom for this 'ContextT' to be runnable.
--
--   [@a@] The value returned from this monad action.
--
newtype ContextT ctx m a =
    ContextT (ReaderT (ContextEnv ctx) (StateT (ContextState ctx) m) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadException, MonadAsyncException)

data ContextEnv ctx = ContextEnv {
    context :: ctx,
    sharedContextData :: SharedContextDatas
  }

data ContextState ctx = ContextState {
    nextName :: Name,
    perWindowState :: PerWindowState ctx,
    lastUsedWin :: Name -- -1 is no window. 0 is the hidden window. 1.. are visible windows
  }

-- | A monad in which shaders are run.
newtype Render a = Render { unRender :: ExceptT String (ReaderT RenderEnv (StateT RenderState IO)) a } deriving (Monad, Applicative, Functor)

data RenderEnv = RenderEnv {
    renderSharedContextData :: SharedContextDatas,
    nonWindowDoAsync :: ContextDoAsync
  }

data RenderState = RenderState {
    perWindowRenderState :: PerWindowRenderState,
    renderWriteTextures :: Set.IntSet,
    renderLastUsedWin :: Name
  }

type Name = Int

type ContextDoAsync = IO () -> IO ()

type PerWindowState ctx = IMap.IntMap (WindowState, ContextWindow ctx) -- -1 is no window. 0 is the hidden window. 1.. are visible windows
type PerWindowRenderState = IMap.IntMap (WindowState, ContextDoAsync)
data WindowState = WindowState {
    windowContextData :: !ContextData
  }

-- | Run a 'Render' monad, that may have the effect of windows or textures being drawn to.
--
--   May throw a 'GPipeException' if a combination of draw images (FBO) used by this render call is unsupported by the graphics driver
render :: (ContextHandler ctx, MonadIO m, MonadException m) => Render () -> ContextT ctx m ()
render (Render m) = do
  void getLastContextWin -- To create hidden window if needed
  ContextT $ do
    ContextEnv ctx cds <- ask
    cs <- lift get
    let wmap' = IMap.map (\(ws,w) -> (ws, contextDoAsync ctx (Just w))) $ perWindowState cs
    (eError, rs) <- liftIO $ runStateT (runReaderT (runExceptT m) (RenderEnv cds (contextDoAsync ctx Nothing))) (RenderState wmap' Set.empty (lastUsedWin cs))
    lift $ put $ cs { lastUsedWin = renderLastUsedWin rs}
    case eError of
      Left s -> liftIO $ throwIO $ GPipeException s
      _ -> return ()

registerRenderWriteTexture :: Int -> Render ()
registerRenderWriteTexture n = Render $ lift $ lift $ modify $ \ rs -> rs { renderWriteTextures = Set.insert n $ renderWriteTextures rs }

instance MonadTrans (ContextT ctx) where
    lift = ContextT . lift . lift

instance MonadIO m => MF.MonadFail (ContextT ctx m) where
    fail = liftIO . MF.fail

-- | Run a 'ContextT' monad transformer that encapsulates an object space.
--   You need an implementation of a 'ContextHandler', which is provided by an auxillary package, such as @GPipe-GLFW@.
runContextT :: (MonadIO m, MonadAsyncException m, ContextHandler ctx) => ContextHandlerParameters ctx -> (forall os. ContextT ctx m a) -> m a
runContextT chp (ContextT m) = do
    cds <- liftIO newContextDatas
    bracket
     (liftIO $ contextHandlerCreate chp)
     (\ctx -> liftIO $ do
       cds' <- readMVar cds
       mapM_ snd cds' -- Delete all windows not explicitly deleted
       contextHandlerDelete ctx
     )
     (\ctx -> evalStateT (runReaderT m (ContextEnv ctx cds)) (ContextState 1 IMap.empty (-1)))

data Window c ds = Window { getWinName :: Name }

instance Eq (Window c ds) where
  (Window a) == (Window b) = a == b

createHiddenWin :: (ContextHandler ctx, MonadIO m) => ContextT ctx m (ContextWindow ctx)
createHiddenWin = ContextT $ do
  ContextEnv ctx cds <- ask
  ContextState wid _ _ <- lift get -- We need to keep next window id and not start over at 1
  w <- liftIO $ createContext ctx Nothing
  cd <- liftIO $ addContextData (contextDelete ctx w) cds
  let ws = WindowState cd
  lift $ put $ ContextState wid (IMap.singleton 0 (ws,w)) 0
  liftIO $ contextDoAsync ctx (Just w) initGlState
  return w

-- | Creates a window
newWindow :: (ContextHandler ctx, MonadIO m) => WindowFormat c ds -> WindowParameters ctx -> ContextT ctx m (Window c ds)
newWindow wf wp = ContextT $ do
  ContextEnv ctx cds <-  ask
  ContextState wid wmap _ <- lift get
  w <- liftIO $ createContext ctx (Just (windowBits wf, wp))
  cd <- liftIO $ addContextData (contextDelete ctx w) cds
  let wid' = wid+1
  let ws = WindowState cd
  lift $ put $ ContextState wid' (IMap.insert wid (ws,w) wmap) wid
  liftIO $ contextDoAsync ctx (Just w) initGlState
  return $ Window wid

-- | Deletes a window. Any rendering to this window will become a noop.
deleteWindow :: (ContextHandler ctx, MonadIO m) => Window c ds -> ContextT ctx m ()
deleteWindow (Window wid) = ContextT $ do
  ContextState nid wmap n <- lift get
  case IMap.lookup wid wmap of
    Nothing -> return ()
    Just (ws, w) -> do
      ContextEnv ctx cds <-  ask
      let wmap' = IMap.delete wid wmap
      n' <- if (IMap.null wmap')
              then do
                void $ let ContextT m = createHiddenWin in m -- Create a hidden window before we delete last window
                return 0 -- The hidden window is now Current
              else if n /= wid then return n
                               else return (fst (head (IMap.toList wmap'))) -- always at least one elem
      liftIO $ do removeContextData cds (windowContextData ws)
                  contextDelete ctx w
      lift $ put $ ContextState nid wmap' n'

initGlState :: IO ()
initGlState = do
  glEnable GL_FRAMEBUFFER_SRGB
  glEnable GL_SCISSOR_TEST
  glPixelStorei GL_PACK_ALIGNMENT 1
  glPixelStorei GL_UNPACK_ALIGNMENT 1

asSync :: (IO () -> IO ()) -> IO x -> IO x
asSync f m = do mutVar <- newEmptyMVar
                f (m >>= putMVar mutVar)
                takeMVar mutVar

getLastContextWin :: (ContextHandler ctx, MonadIO m) => ContextT ctx m (ContextWindow ctx)
getLastContextWin = ContextT $ do
  cs <- lift get
  let wid = lastUsedWin cs
  if wid >= 0
    then return (snd $ perWindowState cs ! wid) -- always exists, since delete context will change lastUsedWin for us
    else let ContextT m = createHiddenWin in m

liftNonWinContextIO :: (ContextHandler ctx, MonadIO m) => IO a -> ContextT ctx m a
liftNonWinContextIO m = do
  ContextEnv ctx _ <- ContextT ask
  w <- getLastContextWin
  ContextT $ liftIO $ asSync (contextDoAsync ctx (Just w)) m

liftNonWinContextAsyncIO :: (ContextHandler ctx, MonadIO m) => IO () -> ContextT ctx m ()
liftNonWinContextAsyncIO m = do
  ContextEnv ctx _ <- ContextT ask
  w <- getLastContextWin
  ContextT $ liftIO $ contextDoAsync ctx (Just w) m


addContextFinalizer :: (ContextHandler ctx, MonadIO m) => IORef a -> IO () -> ContextT ctx m ()
addContextFinalizer k m = ContextT $ do
  ContextEnv ctx _ <- ask
  liftIO $ void $ mkWeakIORef k $ contextDoAsync ctx Nothing m


getLastRenderWin = Render $ do
  rs <- lift $ lift get
  let cwid = renderLastUsedWin rs -- There is always a window available since render calls getLastContextWin
  let (ws, doAsync) = perWindowRenderState rs ! cwid
      cd = windowContextData ws
  return (cwid, cd, doAsync)

-- | Run this action after a 'render' call to swap out the context windows back buffer with the front buffer, effectively showing the result.
--   This call may block if vsync is enabled in the system and/or too many frames are outstanding.
--   After this call, the context window content is undefined and should be cleared at earliest convenience using 'clearContextColor' and friends.
swapWindowBuffers :: (ContextHandler ctx, MonadIO m) => Window c ds -> ContextT ctx m ()
swapWindowBuffers (Window wid) = ContextT $ do
  wmap <- lift $ gets perWindowState
  case IMap.lookup wid wmap of
    Nothing -> return ()
    Just (_, w) -> do
      ctx <- asks context
      liftIO $ contextSwap ctx w


-- | Return the current size of the context frame buffer. This is needed to set viewport size and to get the aspect ratio to calculate projection matrices.
getFrameBufferSize :: (ContextHandler ctx, MonadIO m) => Window c ds -> ContextT ctx m (V2 Int)
getFrameBufferSize (Window wid) = ContextT $ do
  wmap <- lift $ gets perWindowState
  case IMap.lookup wid wmap of
    Nothing -> return $ V2 0 0
    Just (_, w) -> do
      ctx <- asks context
      (x,y) <- liftIO $ contextFrameBufferSize ctx w
      return $ V2 x y

-- | Use the context window handle, which type is specific to the window system used. This handle shouldn't be returned from this function
withContextWindow :: MonadIO m => Window c ds -> (Maybe (ContextWindow ctx) -> IO a) -> ContextT ctx m a
withContextWindow (Window wid) m = ContextT $ do
  wmap <- lift $ gets perWindowState
  liftIO $ m (snd <$> IMap.lookup wid wmap)

-- | This kind of exception may be thrown from GPipe when a GPU hardware limit is reached (for instance, too many textures are drawn to from the same 'FragmentStream')
data GPipeException = GPipeException String
     deriving (Show, Typeable)

instance Exception GPipeException

{-
-- TODO Add async rules
{-# RULES
"liftContextIO >>= liftContextIO >>= x"    forall m1 m2 x.  liftContextIO m1 >>= (\_ -> liftContextIO m2 >>= x) = liftContextIO (m1 >> m2) >>= x
"liftContextIO >>= liftContextIO"          forall m1 m2.    liftContextIO m1 >>= (\_ -> liftContextIO m2) = liftContextIO (m1 >> m2)
  #-}
-}
--------------------------

-- | The reason we need this is that we need to bind a finalizer to a buffer or texture that removes all references VAOs or FBOs from all
--   known ContextData at a future point, where more Contexts may have been created.
type SharedContextDatas = MVar [(ContextData, IO ())] -- IO to delete windows
type ContextData = MVar (VAOCache, FBOCache)
data VAOKey = VAOKey { vaoBname :: !GLuint, vaoCombBufferOffset :: !Int, vaoComponents :: !GLint, vaoNorm :: !Bool, vaoDiv :: !Int } deriving (Eq, Ord)
data FBOKey = FBOKey { fboTname :: !GLuint, fboTlayerOrNegIfRendBuff :: !Int, fboTlevel :: !Int } deriving (Eq, Ord)
data FBOKeys = FBOKeys { fboColors :: [FBOKey], fboDepth :: Maybe FBOKey, fboStencil :: Maybe FBOKey } deriving (Eq, Ord)
type VAOCache = Map.Map [VAOKey] (IORef GLuint)
type FBOCache = Map.Map FBOKeys (IORef GLuint)

getFBOKeys :: FBOKeys -> [FBOKey]
getFBOKeys (FBOKeys xs d s) = xs ++ maybeToList d ++ maybeToList s

newContextDatas :: IO SharedContextDatas
newContextDatas = newMVar []

addContextData :: IO () -> SharedContextDatas -> IO ContextData
addContextData io r = do cd <- newMVar (Map.empty, Map.empty)
                         modifyMVar_ r $ return . ((cd,io):)
                         return cd

removeContextData :: SharedContextDatas -> ContextData -> IO ()
removeContextData r cd = modifyMVar_ r $ return . remove cd
  where remove x ((k,v):xs) | x == k = xs
        remove x (kv:xs)             = kv : remove x xs
        remove _ []                  = []

addCacheFinalizer :: MonadIO m => (GLuint -> (VAOCache, FBOCache) -> (VAOCache, FBOCache)) -> IORef GLuint -> ContextT ctx m ()
addCacheFinalizer f r =  ContextT $ do cds <- asks sharedContextData
                                       liftIO $ do n <- readIORef r
                                                   void $ mkWeakIORef r $ do cs' <- readMVar cds
                                                                             mapM_ (\(cd,_) -> modifyMVar_ cd (return . f n)) cs'

-- | Removes a VAO entry from all SharedContextDatas when one of the buffers are deleted. This will in turn make the VAO finalizer to be run.
addVAOBufferFinalizer :: MonadIO m => IORef GLuint -> ContextT ctx m ()
addVAOBufferFinalizer = addCacheFinalizer deleteVAOBuf
    where deleteVAOBuf n (vao, fbo) = (Map.filterWithKey (\k _ -> all ((/=n) . vaoBname) k) vao, fbo)


-- | Removes a FBO entry from all SharedContextDatas when one of the textures are deleted. This will in turn make the FBO finalizer to be run.
addFBOTextureFinalizer :: MonadIO m => Bool -> IORef GLuint -> ContextT ctx m ()
addFBOTextureFinalizer isRB = addCacheFinalizer deleteVBOBuf
    where deleteVBOBuf n (vao, fbo) = (vao, Map.filterWithKey
                                          (\ k _ ->
                                             all
                                               (\ fk ->
                                                  fboTname fk /= n || isRB /= (fboTlayerOrNegIfRendBuff fk < 0))
                                               $ getFBOKeys k)
                                          fbo)


getVAO :: ContextData -> [VAOKey] -> IO (Maybe (IORef GLuint))
getVAO cd k = do (vaos, _) <- readMVar cd
                 return (Map.lookup k vaos)

setVAO :: ContextData -> [VAOKey] -> IORef GLuint -> IO ()
setVAO cd k v = modifyMVar_ cd $ \ (vaos, fbos) -> return (Map.insert k v vaos, fbos)

getFBO :: ContextData -> FBOKeys -> IO (Maybe (IORef GLuint))
getFBO cd k = do (_, fbos) <- readMVar cd
                 return (Map.lookup k fbos)

setFBO :: ContextData -> FBOKeys -> IORef GLuint -> IO ()
setFBO cd k v = modifyMVar_ cd $ \(vaos, fbos) -> return (vaos, Map.insert k v fbos)
