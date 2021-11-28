{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
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
  -- , FRP.readIO
  , FRP'
  , writeRef'
  , readRef'
  , subscribeTopic'
  , writeTopic'
  , foo
  ) where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad.Identity
import           Control.Monad.Writer
import           Data.Foldable              (for_)
import           Data.Functor.Contravariant
import           Data.Functor.Invariant
import           Data.IORef
import           Data.Void
import           Prelude                    hiding (null)
import           Quartet
import Control.Monad.State (StateT (StateT, runStateT), runState)
import Data.Map hiding (null, empty)
import Data.Dynamic (Dynamic, toDyn, fromDyn, Typeable)


data FRPState = FRPState
  { topics :: Map String Dynamic
  , refs :: Map String Dynamic
  }

instance Show FRPState where
  show = const "FRPState"


type FRP' f a = Static (StateT FRPState IO) f a

foo :: StateT FRPState IO a -> IO a
foo statet = do
  (a, state) <- runStateT statet emptyFRPState
  print state
  return a
  where
    emptyFRPState :: FRPState
    emptyFRPState = FRPState
      { topics = mempty
      , refs = mempty}

type FRP = Static (WriterT [String] Identity)


-- | Ref instantiates CollapseP2P, ExpandS2P
-- i.e. nothing :: Ref ()
-- i.e. |&| :: Ref a -> Ref b -> Ref (a, b)
-- i.e. expand :: Ref (Either a b) -> (Ref a, Ref b)
-- Ref does not instantiate Functor nor Contravariant it's Invariant.
data Ref a = Ref
  { writeRef :: WriteEntity a
  , readRef  :: ReadEntity a
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
  { writeTopic     :: FRP WriteStream a
  , subscribeTopic :: FRP SubscribeStream a
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
newtype WriteEntity a = WriteEntity { runWriteEntity :: Maybe a -> IO () }

instance CollapseP2P WriteEntity where
  nothing = WriteEntity $ \_ -> return ()
  isa |&| isb = WriteEntity $ \mab -> do
    runWriteEntity isa (fst <$> mab)
    runWriteEntity isb (snd <$> mab)

instance ExpandS2P WriteEntity where
  expand w = (WriteEntity $ runWriteEntity w . fmap Left, WriteEntity $ runWriteEntity w . fmap Right)

instance Invariant WriteEntity where
  invmap _ = contramap

instance Contravariant WriteEntity where
  contramap f is = WriteEntity $ runWriteEntity is . fmap f

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

ref :: Typeable a => String -> FRP' Ref a
ref name = Static $ StateT $ \state -> case Data.Map.lookup name (refs state) of
  Nothing -> do
    ioref <- newIORef Nothing
    let ref = Ref
          { writeRef = WriteEntity $ writeIORef ioref
          , readRef = ReadEntity $ readIORef ioref
          }
    return (ref, state { refs = insert name (toDyn ref) (refs state) })
  Just d -> return (fromDyn d undefined, state)

topic :: Typeable a => String -> FRP' Topic a
topic name = Static $ StateT $ \state -> case Data.Map.lookup name (topics state) of
  Nothing -> do
    mvarsRef <- newIORef []
    let topic = Topic
          { writeTopic = Static $ WriterT $ Identity (WriteStream $ \a -> do
              mvars <- readIORef mvarsRef
              for_ mvars $ \mvar -> putMVar mvar a, [name])
          , subscribeTopic = Static $ WriterT $ Identity (SubscribeStream $ \action -> do
              mvar <- newEmptyMVar
              modifyIORef mvarsRef (mvar:)
              void $ forkIO $ forever $ takeMVar mvar >>= action, [name])
          }
    return (topic, state { topics = insert name (toDyn topic) (topics state)})
  Just d -> return (fromDyn d undefined, state)

writeEntity :: FRP WriteEntity a -> Maybe a -> IO ()
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
connect subscribeStream writeEntity = runSubscibeStream subscribeStream (runWriteEntity writeEntity . Just)

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

--

writeRef' :: FRP' Ref a -> FRP' WriteEntity a
writeRef' r = Static $ writeRef <$> runStatic r

readRef' :: FRP' Ref a -> FRP' ReadEntity a
readRef' r = Static $ readRef <$> runStatic r


subscribeTopic' :: FRP' Topic a -> FRP' SubscribeStream a
subscribeTopic' = undefined

writeTopic' :: FRP' Topic a -> FRP' WriteStream a
writeTopic' = undefined
