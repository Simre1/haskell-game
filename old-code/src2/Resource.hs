{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
module Resource where

import Import
import Sigma

withResource :: Monad m => Resource m r -> (r -> Signal m a (Maybe b)) -> Signal m a b -> Signal m a b
withResource (Resource g f t) s1 s2 = Signal $ \a -> do
  r <- g
  let signal s1' s2' = Signal $ \a' -> do
        (maybeB, cont) <- stepSignal s1' a'
        maybe
          (stepSignal (Signal $ \a'' -> f r >> stepSignal s2' a'') a')
          (pure . ( , signal cont s2'))
          maybeB
  stepSignal (signal (s1 $ t r) s2) a

data Resource m b = forall a. Resource {getResource :: m a, freeResource :: a -> m (),  transformResource :: a -> b}

newResource :: m a -> (a -> m ()) -> Resource m a
newResource g f = Resource g f id

instance Functor (Resource m) where
  fmap f (Resource ma am ab) = Resource ma am (f . ab)

instance Applicative m => Applicative (Resource m) where
  pure a = Resource (pure a) (\_ -> pure ()) id
  (Resource get1 free1 trans1) <*> (Resource get2 free2 trans2) = Resource get free trans
    where get = (,) <$> get1 <*> get2
          free (v1,v2) = free1 v1 <* free2 v2
          trans (v1,v2) = trans1 v1 (trans2 v2)

instance HFunctor Resource where
  hmap morph (Resource g f t) = Resource (morph g) (morph . f) t
