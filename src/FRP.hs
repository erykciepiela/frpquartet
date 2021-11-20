module FRP
  ( Ref (..)
  , ref
  , ReadEntity (..)
  , Write (..)
  , Topic (..)
  , topic
  , ReadStream (..)
  , write
  , FRP.read
  , subscribe
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

-- | Ref instantiates P2P
-- i.e. |&| :: Ref a -> Ref b -> Ref (a, b)
-- Ref does not instantiate Functor nor Contravariant it's Invariant.
data Ref a = Ref
  { writeRef :: Static (WriterT [String] Identity) Write a
  , readRef  :: Static (WriterT [String] Identity) ReadEntity a
  }

instance P2P Ref where
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
    { writeRef = g >$< writeRef ref
    , readRef = f <$> readRef ref
    }

-- | Topic instatiates P2S
-- i.e. ||| :: Topic a -> Topic b -> Topic (Either a b)
-- Topic does not instantiate Functor nor Contravariant, it's Invariant
data Topic a = Topic
  { writeTopic :: Static (WriterT [String] Identity) Write a
  , readTopic  :: Static (WriterT [String] Identity) ReadStream a
  }

instance P2S Topic where
  never = Topic
    { writeTopic = never
    , readTopic = never
    }
  ea ||| eb = Topic
    { writeTopic = writeTopic ea ||| writeTopic eb
    , readTopic = readTopic ea ||| readTopic eb
    }

instance Invariant Topic where
  invmap f g topic = Topic
    { writeTopic = g >$< writeTopic topic
    , readTopic = f <$> readTopic topic
    }

-- | Write instantiates P2S, P2P and Contravariant
newtype Write a = Write { runWrite :: a -> IO () }

instance P2S Write where
  never = Write $ \_ -> return ()
  sa ||| sb = Write $ \aorb -> either (runWrite sa) (runWrite sb) aorb

instance P2P Write where
  nothing = Write $ \_ -> return ()
  isa |&| isb = Write $ \(a, b) -> do
    runWrite isa a
    runWrite isb b

instance Contravariant Write where
  contramap f is = Write $ runWrite is . f

-- | ReadEntity instantiates P2P and Functor
newtype ReadEntity a = ReadEntity { runReadEntity :: IO a }

instance P2P ReadEntity where
  nothing = ReadEntity $ return ()
  oea |&| oeb = ReadEntity $ (,) <$> runReadEntity oea <*> runReadEntity oeb

instance Functor ReadEntity where
  fmap f oe = ReadEntity $ f <$> runReadEntity oe


-- | ReadStream instantiates P2S and Functor
newtype ReadStream a = ReadStream { runReadStream :: (a -> IO ()) -> IO () }

instance P2S ReadStream where
  never = ReadStream $ \_ -> return ()
  osa ||| osb = ReadStream $ \aorb2io -> do
    runReadStream osa $ aorb2io . Left
    runReadStream osb $ aorb2io . Right

instance Functor ReadStream where
  fmap f os = ReadStream $ \b2io -> runReadStream os (b2io . f)

--

ref :: String -> a -> IO (Ref a)
ref name a = do
  ref <- newIORef a
  return $ Ref
    { writeRef = Static $ WriterT $ Identity (Write $ writeIORef ref, [name])
    , readRef = Static $ WriterT $ Identity (ReadEntity $ readIORef ref, [name])
    }

type ReadIO a = IO a

readIO :: String -> ReadIO a -> Static (WriterT [String] Identity) ReadEntity a
readIO name ioa = Static $ WriterT $ Identity (ReadEntity ioa, [name])

topic :: String -> IO (Topic a)
topic name = do
  mvarsRef <- newIORef []
  return $ Topic
    { writeTopic = Static $ WriterT $ Identity (Write $ \a -> do
        mvars <- readIORef mvarsRef
        for_ mvars $ \mvar -> putMVar mvar a, [name])
    , readTopic = Static $ WriterT $ Identity (ReadStream $ \action -> do
        mvar <- newEmptyMVar
        modifyIORef mvarsRef (mvar:)
        void $ forkIO $ forever $ takeMVar mvar >>= action, [name])
    }

write :: Static (WriterT [String] Identity) Write a -> a -> IO ()
write siea a = let (doWriteEntity, meta) = (runIdentity . runWriterT . runStatic) siea
                in do
                  print meta
                  runWrite doWriteEntity a

read :: Static (WriterT [String] Identity) ReadEntity a -> IO a
read siea = let (doReadEntity, meta) = (runIdentity . runWriterT . runStatic) siea
            in do
              print meta
              runReadEntity doReadEntity

subscribe :: Static (WriterT [String] Identity) ReadStream a -> (a -> IO ()) -> IO ()
subscribe siea callback = let (doReadStream, meta) = (runIdentity . runWriterT . runStatic) siea
            in do
              print meta
              runReadStream doReadStream callback

-- last but not least, things for free (these functions are not exported, it's just for documentation)

_nothingRef :: Ref ()
_nothingRef = nothing

_nothingReadEntity :: ReadEntity ()
_nothingReadEntity = nothing

_nothingWrite :: Write ()
_nothingWrite = nothing

_constantReadEntity :: a -> ReadEntity a
_constantReadEntity = constant

_nullWriteEntity :: Write a
_nullWriteEntity = null

_nullWriteStream :: Write a
_nullWriteStream = null

_neverTopic :: Topic Void
_neverTopic = never

_neverReadStream :: ReadStream Void
_neverReadStream = never

_neverWrite :: Write Void
_neverWrite = never

_emptyReadStream :: ReadStream a
_emptyReadStream = empty

-- you cannot create ad-hoc arbitrary contravariant P2S
-- if you need to create ad-hoc arbitrary contravariant you have to use P2P and @null@
_nullP2S :: (Contravariant f, P2S f) => (a -> Void) -> f a
_nullP2S f = f >$< never
