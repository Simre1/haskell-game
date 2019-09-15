{-# LANGUAGE AllowAmbiguousTypes #-}
module WriterSpec where

import Test.Hspec

import qualified Control.Concurrent.Async as A
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (evaluate)

import Polysemy
import Polysemy.Async
import Polysemy.Error
import Polysemy.Writer

censor' :: forall e s a r
        . (Member (Error e) r, Member (Writer s) r)
        => (s -> s)
        -> Sem r a
        -> Sem r a
censor' f m = do
  res <- censor f $ fmap Right m `catch` (pure . Left)
  case res of
      Right res' -> return res'
      Left e -> throw (e :: e)

test1 :: (String, Either () ())
test1 =
    run
  . runWriter
  . runError $
  do
    tell "censoring"
    censor @String
      (drop 4)
      (tell " not applied" *> throw ())
    `catch`
      (\(_ :: ()) -> pure ())

test2 :: (String, Either () ())
test2 =
    run
  . runWriter
  . runError $
  do
    tell "censoring"
    censor' @() @String
      (drop 4)
      (tell " not applied" *> throw ())
    `catch`
      (\(_ :: ()) -> pure ())

test3 :: (String, (String, ()))
test3 = run . runWriter $ listen (tell "and hear")

test4 :: IO (String, String)
test4 = do
  tvar <- newTVarIO ""
  (listened, _) <- runFinal . asyncToIOFinal . runWriterTVar tvar $ do
    tell "message "
    listen $ do
      tell "has been"
      a <- async $ tell " received"
      await a
  end <- readTVarIO tvar
  return (end, listened)

test5 :: IO (String, String)
test5 = do
  tvar <- newTVarIO ""
  lock <- newEmptyMVar
  (listened, a) <- runFinal . asyncToIOFinal . runWriterTVar tvar $ do
    tell "message "
    listen $ do
      tell "has been"
      a <- async $ do
        embedFinal $ takeMVar lock
        tell " received"
      return a
  putMVar lock ()
  _ <- A.wait a
  end <- readTVarIO tvar
  return (end, listened)

test6 :: Sem '[Error (A.Async (Maybe ())), Final IO] String
test6 = do
  tvar <- embedFinal $ newTVarIO ""
  lock <- embedFinal $ newEmptyMVar
  let
      inner = do
        tell "message "
        fmap snd $ listen @String $ do
          tell "has been"
          a <- async $ do
            embedFinal $ takeMVar lock
            tell " received"
          throw a
  asyncToIOFinal (runWriterTVar tvar inner) `catch` \a ->
    embedFinal $ do
      putMVar lock ()
      (_ :: Maybe ()) <- A.wait a
      readTVarIO tvar



spec :: Spec
spec = do
  describe "writer" $ do
    it "should not censor" $ do
      test1 `shouldBe` ("censoring not applied", Right ())

    it "should censor" $ do
      test2 `shouldBe` ("censoring applied", Right ())

    it "should have a proper listen" $ do
      test3 `shouldBe` ("and hear", ("and hear", ()))

    it "should be strict in the output" $
      let
        t1 = runWriter @String $ do
          tell @String (error "strict")
          return ()

        t2 = runWriter @String $ do
          _ <- listen @String (tell @String (error "strict"))
          return ()

        t3 = runWriter @String $ do
          pass @String $ pure (\_ -> error "strict", ())
          return ()
      in do
        runM t1           `shouldThrow` errorCall "strict"
        evaluate (run t1) `shouldThrow` errorCall "strict"
        runM t2           `shouldThrow` errorCall "strict"
        evaluate (run t2) `shouldThrow` errorCall "strict"
        runM t3           `shouldThrow` errorCall "strict"
        evaluate (run t3) `shouldThrow` errorCall "strict"

  describe "runWriterTVar" $ do
    it "should listen and commit asyncs spawned and awaited upon in a listen \
       \block" $ do
      (end, listened) <- test4
      end `shouldBe` "message has been received"
      listened `shouldBe` "has been received"

    it "should commit writes of asyncs spawned inside a listen block even if \
       \the block has finished." $ do
      (end, listened) <- test5
      end `shouldBe` "message has been received"
      listened `shouldBe` "has been"


    it "should commit writes of asyncs spawned inside a listen block even if \
       \the block failed for any reason." $ do
      Right end1 <- runFinal . errorToIOFinal $ test6
      Right end2 <- runFinal . runError $ test6
      end1 `shouldBe` "message has been received"
      end2 `shouldBe` "message has been received"
