{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Window.GPipe
  ( WindowIO
  , WindowAction
  , makeWindowAction
  , executeWindowAction
  , runGPipe
  , module GPipe
  )
  where

import Graphics.GPipe as GPipe
import Polysemy (interpret, Member, Sem, makeSem, runM)
import Polysemy.Embed (Embed, embed)
import Polysemy.IO (embedToMonadIO)

import Sigma (Signal, signalMorph, reactimateUntilTrue)

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
