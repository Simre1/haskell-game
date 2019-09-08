{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}


module GPipe.Interface (WindowIO, WindowAction, runGPipe, makeWindowAction, getWindow, executeWindowAction) where

import Polysemy
import Graphics.GPipe
import Control.Concurrent
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad
import Data.Functor ((<&>), ($>))
import Data.Text
import qualified Data.Map.Strict as M
import Control.Concurrent.MVar
import Unsafe.Coerce
import Data.Coerce
import Polysemy.Reader
import Polysemy.State
import Polysemy.IO
import Sigma
import Polysemy.Embed

data WindowAction ctx c ds a = WindowAction (Window c ds -> ContextT ctx IO a) deriving (Functor)

instance Applicative (WindowAction ctx c ds) where
  pure a = WindowAction $ \_ -> pure a
  (WindowAction action1) <*> (WindowAction action2) = WindowAction $ \w -> action1 w <*> action2 w

instance Monad (WindowAction ctx c ds) where
  (WindowAction action) >>= f = WindowAction $ \w -> do
    a <- action w
    let WindowAction action2 = f a
    action2 w

makeWindowAction :: (Window c ds -> ContextT ctx IO a) -> WindowAction ctx c ds a
makeWindowAction action = WindowAction action

getWindow :: WindowAction ctx c ds (Window c ds)
getWindow = WindowAction $ \w -> pure w

data WindowIO ctx c ds (m :: * -> *) a where
  ExecuteWindowAction :: WindowAction ctx c ds a -> WindowIO ctx c ds m a

makeSem ''WindowIO

runGPipeEffect :: forall ctx c ds r a. Member (Embed (ContextT ctx IO)) r => Window c ds -> Sem (WindowIO ctx c ds:r) a -> Sem r a
runGPipeEffect window = interpret $ \case
    ExecuteWindowAction (WindowAction action) ->
      embed $ action window


runGPipe :: forall ctx c ds. (ContextHandler ctx) => ContextHandlerParameters ctx -> WindowFormat c ds -> WindowParameters ctx -> Signal (Sem [WindowIO ctx c ds, Embed IO,Embed (ContextT ctx IO)]) Bool -> IO ()
runGPipe contextParameters windowFormat windowParameters sig = do
  runContextT contextParameters $ do
    window <- newWindow windowFormat windowParameters
    runM $ reactimateUntilTrue $ signalMorph (embedToMonadIO @(ContextT ctx IO) . runGPipeEffect window) sig
  --
  -- where
  --   run :: Window os c ds -> Sem [WindowIO ctx c ds, Embed IO, Embed (SimpleContext ctx)] a -> Sem [Embed IO, Embed (SimpleContext ctx)] a
  --   run =
