{-# LANGUAGE TupleSections #-}
module Sigma.Signal
  ( Signal (..)
  , arrM
  , arrAction
  , signalMorph
  , signalSimpleMorph
  , feedback
  , simpleFeedback
  , doOnce
  , withInitialization
  , withConstantInput
  , buildSignal) where

import Prelude hiding ((.), id)
import Data.Functor
import Control.Monad
import Control.Arrow
import Control.Applicative
import Control.Category

newtype Signal m a b = Signal {stepSignal :: a -> m (b, Signal m a b)}

instance Functor m => Functor (Signal m a) where
  fmap f (Signal step) = Signal $ fmap (\(b,cont) -> (f b, fmap f cont)) . step
  {-# INLINABLE fmap #-}

instance Applicative m => Applicative (Signal m a) where
  pure a = Signal (const $ pure (a,pure a))
  (Signal step1) <*> (Signal step2) = Signal $ \a -> liftA2 combine (step1 a) (step2 a)
    where combine (f, fCont) (a, aCont) = (f a, fCont <*> aCont)
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}

instance Monad m => Category (Signal m) where
  id = Signal (\a -> pure (a, id))
  (Signal step2) . Signal (step1) = Signal $ \a -> do
    (b, cont1) <- step1 a
    (c, cont2) <- step2 b
    return (c, cont2 . cont1)
  {-# INLINABLE id #-}

instance (Monad m) => Arrow (Signal m) where
  arr f = Signal $ \a -> pure (f a, arr f)
  first (Signal step) = Signal $ \(a,c) ->
    (\(b, cont) -> ((b,c), first cont)) <$> step a

  {-# INLINABLE arr #-}
  {-# INLINABLE first #-}

instance Monad m => ArrowChoice (Signal m) where
  left (Signal step) = Signal $ \eitherAD -> case eitherAD of
    (Left a) -> do
      (b,cont) <- step a
      return (Left b, left cont)
    (Right c) -> return (Right c, left $ Signal step)
  {-# INLINABLE left #-}

instance MonadPlus m => ArrowZero (Signal m) where
  zeroArrow = Signal $ const mzero
  {-# INLINABLE zeroArrow #-}

instance MonadPlus m => ArrowPlus (Signal m) where
  signal1 <+> signal2 = Signal $ \a -> stepSignal signal1 a `mplus` stepSignal signal2 a
  {-# INLINABLE (<+>) #-}

instance Alternative m => Alternative (Signal m a) where
  empty = Signal $ const empty
  signal1 <|> signal2 = Signal $ \a -> stepSignal signal1 a <|> stepSignal signal2 a
  {-# INLINABLE empty #-}
  {-# INLINABLE (<|>) #-}

buildSignal :: (a -> m (b, Signal m a b)) -> Signal m a b
buildSignal = Signal

arrM :: Functor m => (a -> m b) -> Signal m a b
arrM f = Signal $ fmap (,arrM f) . f
{-# INLINABLE arrM #-}

arrAction :: Functor m => m b -> Signal m a b
arrAction = arrM . const
{-# INLINABLE arrAction #-}


signalMorph :: Functor m2
        => (forall c . (a1 -> m1 (b1, c)) -> (a2 -> m2 (b2, c)))
        -> Signal m1 a1 b1
        -> Signal m2 a2 b2
signalMorph f signal = Signal $ fmap (second (signalMorph f)) . f (stepSignal signal)
{-# INLINABLE signalMorph #-}


signalSimpleMorph :: Functor m2 => (forall x. m1 x -> m2 x) -> Signal m1 a b -> Signal m2 a b
signalSimpleMorph f = signalMorph (f.)
{-# INLINABLE signalSimpleMorph #-}


feedback :: Monad m => c -> Signal m (a,c) (b,c) -> Signal m a b
feedback initial (Signal step) = Signal $ \a -> do
  ((b,c), cont) <- step (a,initial)
  return (b, feedback c cont)
{-# INLINABLE feedback #-}


simpleFeedback :: Functor m => a -> Signal m a a -> Signal m () a
simpleFeedback initial signal = Signal $ \_ ->
  stepSignal signal initial <&> \(a,c) -> (a, simpleFeedback a c)
{-# INLINABLE simpleFeedback #-}

doOnce :: Applicative m => m a -> Signal m () a
doOnce action = Signal . const $
  (\a -> (a, arrM (const $ pure a))) <$> action

withInitialization :: Monad m => m x -> (x -> Signal m a b) -> Signal m a b
withInitialization initialize f = Signal $ \a -> do
  x <- initialize
  stepSignal (f x) a

withConstantInput :: Functor m => a -> Signal m a b -> Signal m x b
withConstantInput input sig = Signal $ \_ ->
  second (withConstantInput input) <$> (stepSignal sig input)
