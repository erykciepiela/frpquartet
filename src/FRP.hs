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
  , subscribeStream
  , writeStream
  , FRP.readIO
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
-- i.e. nothing :: Ref ()
-- i.e. |&| :: Ref a -> Ref b -> Ref (a, b)
-- i.e. expand :: Ref (Either a b) -> (Ref a, Ref b)
-- Ref does not instantiate Functor nor Contravariant it's Invariant.
data Ref a = Ref
  { writeRef :: FRP WriteEntity a
  , readRef  :: FRP ReadEntity a
  }

instance ExpandS2P Ref where
  expand r = (Ref
    { writeRef = fst . expand $ writeRef r
    , readRef = fst . expand $ readRef r
    }, Ref
    { writeRef = snd . expand $ writeRef r
    , readRef = snd . expand $ readRef r
    })

instance CollapseP2P Ref where
  nothing = Ref
    { writeRef = nothing
    , readRef = nothing
    }
  ea |&| eb = Ref
    { writeRef = writeRef ea |&| writeRef eb
    , readRef = readRef ea |&| readRef eb
    }

instance Invariant Ref where
  invmap f g ref = Ref
    { writeRef = invmap f g $ writeRef ref
    , readRef = invmap f g $ readRef ref
    }

-- | Topic instatiates CollapseP2S, ExpandS2P
-- i.e. never :: Topic Void
-- i.e. ||| :: Topic a -> Topic b -> Topic (Either a b)
-- i.e. expand :: Topic (Either a b) -> (Topic a, Topic b)
-- Topic does not instantiate Functor nor Contravariant, it's Invariant
data Topic a = Topic
  { writeTopic :: FRP WriteStream a
  , subscribeTopic  :: FRP SubscribeStream a
  }

instance ExpandS2P Topic where
  expand t = (Topic
    { writeTopic = fst . expand $ writeTopic t
    , subscribeTopic = fst . expand $ subscribeTopic t
    }, Topic
    { writeTopic = snd . expand $ writeTopic t
    , subscribeTopic = snd . expand $ subscribeTopic t
    })

instance CollapseP2S Topic where
  never = Topic
    { writeTopic = never
    , subscribeTopic = never
    }
  ea ||| eb = Topic
    { writeTopic = writeTopic ea ||| writeTopic eb
    , subscribeTopic = subscribeTopic ea ||| subscribeTopic eb
    }

instance Invariant Topic where
  invmap f g topic = Topic
    { writeTopic = invmap f g $ writeTopic topic
    , subscribeTopic = invmap f g $ subscribeTopic topic
    }

-- | WriteEntity instantiates CollapseP2P, ExpandS2P and Contravariant
newtype WriteEntity a = WriteEntity { runWriteEntity :: a -> IO () }

instance CollapseP2P WriteEntity where
  nothing = WriteEntity $ \_ -> return ()
  isa |&| isb = WriteEntity $ \(a, b) -> do
    runWriteEntity isa a
    runWriteEntity isb b

instance ExpandS2P WriteEntity where
  expand w = (WriteEntity $ runWriteEntity w . Left, WriteEntity $ runWriteEntity w . Right)

instance Invariant WriteEntity where
  invmap _ = contramap

instance Contravariant WriteEntity where
  contramap f is = WriteEntity $ runWriteEntity is . f

-- | WriteStream instantiates CollapseP2S, ExpandS2P and Contravariant
newtype WriteStream a = WriteStream { runWriteStream :: a -> IO () }

instance CollapseP2S WriteStream where
  never = WriteStream $ \_ -> return ()
  isa ||| isb = WriteStream $ either (runWriteStream isa) (runWriteStream isb)

instance ExpandS2P WriteStream where
  expand w = (WriteStream $ runWriteStream w . Left, WriteStream $ runWriteStream w . Right)


instance Invariant WriteStream where
  invmap _ = contramap

instance Contravariant WriteStream where
  contramap f is = WriteStream $ runWriteStream is . f

-- | ReadEntity instantiates CollapseP2P, ExpandS2P and Functor
newtype ReadEntity a = ReadEntity { runReadEntity :: IO (Maybe a) }

instance CollapseP2P ReadEntity where
  nothing = ReadEntity $ return (Just ())
  oea |&| oeb = ReadEntity $ (\ma mb -> (,) <$> ma <*> mb) <$> runReadEntity oea <*> runReadEntity oeb

instance ExpandS2P ReadEntity where
  expand re = (ReadEntity $ maybe Nothing (either Just (const Nothing)) <$> runReadEntity re, ReadEntity $ maybe Nothing (either (const Nothing) Just) <$> runReadEntity re)

instance Invariant ReadEntity where
  invmap f _ readEntity = fmap f readEntity

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
  expand ss = (SubscribeStream $ \a2io -> runSubscibeStream ss $ \aorb -> either a2io (const (return ())) aorb, SubscribeStream $ \b2io -> runSubscibeStream ss $ \aorb -> either (const (return ())) b2io aorb)

instance Functor SubscribeStream where
  fmap f os = SubscribeStream $ \b2io -> runSubscibeStream os (b2io . f)

instance Invariant SubscribeStream where
  invmap f _ subscribeStream = fmap f subscribeStream

--

ref :: String -> IO (Ref a)
ref name = do
  ref <- newIORef Nothing
  return $ Ref
    { writeRef = Static $ WriterT $ Identity (WriteEntity $ writeIORef ref . Just, [name])
    , readRef = Static $ WriterT $ Identity (ReadEntity $ readIORef ref, [name])
    }

type ReadIO a = IO a

readIO :: String -> ReadIO a -> FRP ReadEntity a
readIO name ioa = Static $ WriterT $ Identity (ReadEntity (Just <$> ioa), [name])

topic :: String -> IO (Topic a)
topic name = do
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

subscribeStream :: FRP SubscribeStream a -> (a -> IO ()) -> IO ()
subscribeStream siea callback = let (doReadStream, meta) = (runIdentity . runWriterT . runStatic) siea
            in do
              print meta
              runSubscibeStream doReadStream callback

connect :: SubscribeStream a -> WriteEntity a -> IO ()
connect subscribeStream writeEntity = runSubscibeStream subscribeStream (runWriteEntity writeEntity)

-- last but not least, things for free (these functions are not exported, it's just for documentation)

_nothingRef :: Ref ()
_nothingRef = nothing

_nothingReadEntity :: ReadEntity ()
_nothingReadEntity = nothing

_nothingWriteEntity :: WriteEntity ()
_nothingWriteEntity = nothing

_constantReadEntity :: a -> ReadEntity a
_constantReadEntity = constant

_nullWriteEntity :: WriteEntity a
_nullWriteEntity = null

_neverTopic :: Topic Void
_neverTopic = never

_neverWriteStream :: WriteStream Void
_neverWriteStream = never

_emptySubscribeStream :: SubscribeStream a
_emptySubscribeStream = empty
