{-# LANGUAGE LambdaCase #-}
module FRP
  ( Ref (..)
  , ref
  , Topic (..)
  , topic
  , ReadEntity (..)
  , WriteEntity (..)
  , WriteStream (..)
  , SubscribeStream (..)
  , readEntity
  , writeEntity
  , subscribe
  , writeStream
  , FRP.readIO

  , testRead

  , FRP
  ) where

import Quartet
import Prelude hiding (null)
import Data.Functor.Contravariant
import Data.Void
import Control.Monad.Writer
import Control.Monad.Identity
import Data.IORef
import Data.Foldable (for_)
import Control.Concurrent.MVar
import Control.Concurrent
import Data.Functor.Invariant (Invariant (invmap))

type FRP = Static (WriterT [String] Identity)

-- | Ref instantiates CollapseP2P, ExpandS2P
-- i.e. |&| :: Ref a -> Ref b -> Ref (a, b)
-- i.e. expand :: Ref (Either a b) -> (Ref a, Ref b)
-- Ref does not instantiate Functor nor Contravariant it's Invariant.
data Ref a = Ref
  { writeRef :: FRP WriteEntity a
  , readRef  :: FRP ReadEntity a
  }

instance CollapseP2P Ref where
  nothing = Ref
    { writeRef = nothing
    , readRef = nothing
    }
  ea |&| eb = Ref
    { writeRef = writeRef ea |&| writeRef eb
    , readRef = readRef ea |&| readRef eb
    }
instance ExpandS2P Ref where
  foo = Ref
    { writeRef = foo
    , readRef = foo
    }
  expand r = (Ref
    { writeRef = fst . expand $ writeRef r
    , readRef = fst . expand $ readRef r
    }, Ref
    { writeRef = snd . expand $ writeRef r
    , readRef = snd . expand $ readRef r
    })

instance Invariant Ref where
  invmap f g ref = Ref
    { writeRef = g >$< writeRef ref
    , readRef = f <$> readRef ref
    }

-- | Topic instatiates CollapseP2S, ExpandS2P
-- i.e. ||| :: Topic a -> Topic b -> Topic (Either a b)
-- i.e. expand :: Topic (Either a b) -> (Topic a, Topic b)
-- Topic does not instantiate Functor nor Contravariant, it's Invariant
data Topic a = Topic
  { writeTopic :: FRP WriteStream a
  , subscribeTopic  :: FRP SubscribeStream a
  }

instance CollapseP2S Topic where
  never = Topic
    { writeTopic = foo
    , subscribeTopic = never
    }
  ea ||| eb = Topic
    { writeTopic = writeTopic ea ||| writeTopic eb
    , subscribeTopic = subscribeTopic ea ||| subscribeTopic eb
    }

instance ExpandS2P Topic where
  foo = Topic
    { writeTopic = foo
    , subscribeTopic = foo
    }
  expand t = (Topic
    { writeTopic = fst . expand $ writeTopic t
    , subscribeTopic = fst . expand $ subscribeTopic t
    }, Topic
    { writeTopic = snd . expand $ writeTopic t
    , subscribeTopic = snd . expand $ subscribeTopic t
    })

instance Invariant Topic where
  invmap f g topic = Topic
    { writeTopic = g >$< writeTopic topic
    , subscribeTopic = f <$> subscribeTopic topic
    }

-- | WriteEntity instantiates CollapseP2P, ExpandS2P and Contravariant
newtype WriteEntity a = WriteEntity { runWriteEntity :: a -> IO () }

instance CollapseP2P WriteEntity where
  nothing = WriteEntity $ \_ -> return ()
  isa |&| isb = WriteEntity $ \(a, b) -> do
    runWriteEntity isa a
    runWriteEntity isb b

instance ExpandS2P WriteEntity where
  foo = WriteEntity $ \_ -> return ()
  expand w = (WriteEntity $ runWriteEntity w . Left, WriteEntity $ runWriteEntity w . Right)

instance Contravariant WriteEntity where
  contramap f is = WriteEntity $ runWriteEntity is . f

-- | WriteStream instantiates CollapseP2S, ExpandS2P and Contravariant
newtype WriteStream a = WriteStream { runWriteStream :: a -> IO () }

instance CollapseP2S WriteStream where
  never = WriteStream $ \_ -> return ()
  isa ||| isb = WriteStream $ either (runWriteStream isa) (runWriteStream isb)

instance ExpandS2P WriteStream where
  foo = WriteStream $ \_ -> return ()
  expand w = (WriteStream $ runWriteStream w . Left, WriteStream $ runWriteStream w . Right)

instance Contravariant WriteStream where
  contramap f is = WriteStream $ runWriteStream is . f

-- | ReadEntity instantiates CollapseP2P, ExpandS2P and Functor
newtype ReadEntity a = ReadEntity { runReadEntity :: IO (Maybe a) }

instance CollapseP2P ReadEntity where
  nothing = ReadEntity $ return (Just ())
  oea |&| oeb = ReadEntity $ (\ma mb -> (,) <$> ma <*> mb) <$> runReadEntity oea <*> runReadEntity oeb

instance ExpandS2P ReadEntity where
  foo = ReadEntity $ return Nothing
  expand re = (ReadEntity $ maybe Nothing (either Just (const Nothing)) <$> runReadEntity re, ReadEntity $ maybe Nothing (either (const Nothing) Just) <$> runReadEntity re)

instance Functor ReadEntity where
  fmap f oe = ReadEntity $ fmap f <$> runReadEntity oe

instance Applicative ReadEntity where
  pure = constant
  ref <*> rea = (\(f, a) -> f a) <$> (ref |&| rea)

-- | SubscribeStream instantiates CollapseP2S, ExpandS2P and Functor
newtype SubscribeStream a = SubscribeStream { runSubscibeStream :: (a -> IO ()) -> IO () }

instance CollapseP2S SubscribeStream where
  never = SubscribeStream $ \_ -> return ()
  osa ||| osb = SubscribeStream $ \aorb2io -> do
    runSubscibeStream osa $ aorb2io . Left
    runSubscibeStream osb $ aorb2io . Right

instance ExpandS2P SubscribeStream where
  foo = SubscribeStream $ \_ -> return ()
  expand ss = (SubscribeStream $ \a2io -> runSubscibeStream ss $ \aorb -> either a2io (const (return ())) aorb, SubscribeStream $ \b2io -> runSubscibeStream ss $ \aorb -> either (const (return ())) b2io aorb)

instance Functor SubscribeStream where
  fmap f os = SubscribeStream $ \b2io -> runSubscibeStream os (b2io . f)

--

ref :: String -> Static IO Ref a
ref name = Static $ do
  ref <- newIORef Nothing
  return $ Ref
    { writeRef = Static $ WriterT $ Identity (WriteEntity $ writeIORef ref . Just, [name])
    , readRef = Static $ WriterT $ Identity (ReadEntity $ readIORef ref, [name])
    }

type ReadIO a = IO a

readIO :: String -> ReadIO a -> FRP ReadEntity a
readIO name ioa = Static $ WriterT $ Identity (ReadEntity (Just <$> ioa), [name])

topic :: String -> Static IO Topic a
topic name = Static $ do
  mvarsRef <- newIORef []
  return $ Topic
    { writeTopic = Static $ WriterT $ Identity (WriteStream $ \a -> do
        mvars <- readIORef mvarsRef
        for_ mvars $ \mvar -> putMVar mvar a, [name])
    , subscribeTopic = Static $ WriterT $ Identity (SubscribeStream $ \action -> do
        mvar <- newEmptyMVar
        modifyIORef mvarsRef (mvar:)
        void $ forkIO $ forever $ takeMVar mvar >>= action, [name])
    }

writeEntity :: FRP WriteEntity a -> a -> IO ()
writeEntity siea a = let (doWriteEntity, meta) = (runIdentity . runWriterT . runStatic) siea
                in do
                  print meta
                  runWriteEntity doWriteEntity a

writeStream :: FRP WriteStream a -> a -> IO ()
writeStream siea a = let (doWriteStream, meta) = (runIdentity . runWriterT . runStatic) siea
                in do
                  print meta
                  runWriteStream doWriteStream a

readEntity :: FRP ReadEntity a -> IO (Maybe a)
readEntity siea = let (doReadEntity, meta) = (runIdentity . runWriterT . runStatic) siea
            in do
              print meta
              runReadEntity doReadEntity

subscribe :: FRP SubscribeStream a -> (a -> IO ()) -> IO ()
subscribe siea callback = let (doReadStream, meta) = (runIdentity . runWriterT . runStatic) siea
            in do
              print meta
              runSubscibeStream doReadStream callback

-- last but not least, things for free (these functions are not exported, it's just for documentation)

_nothingRef :: Ref ()
_nothingRef = nothing

_nothingReadEntity :: ReadEntity ()
_nothingReadEntity = nothing

_nothingWrite :: WriteEntity ()
_nothingWrite = nothing

_constantReadEntity :: a -> ReadEntity a
_constantReadEntity = constant

_nullWriteEntity :: WriteEntity a
_nullWriteEntity = null

_nullWriteStream :: WriteEntity a
_nullWriteStream = null

_neverTopic :: Topic Void
_neverTopic = never

_neverReadStream :: SubscribeStream Void
_neverReadStream = never

_neverWrite :: WriteEntity Void
_neverWrite = foo

_emptyReadStream :: SubscribeStream a
_emptyReadStream = empty

-- you cannot create ad-hoc arbitrary contravariant CollapseP2S
-- if you need to create ad-hoc arbitrary contravariant you have to use CollapseP2P and @null@
_nullP2S :: (Contravariant f, CollapseP2S f) => (a -> Void) -> f a
_nullP2S f = f >$< never


--

testRead :: Show a => String -> FRP ReadEntity a -> IO ()
testRead prompt read = do
  readEntity read >>= putStrLn . ((prompt <> ": ") <>) . show
  putStrLn "(press to continue...)"
  void getLine
