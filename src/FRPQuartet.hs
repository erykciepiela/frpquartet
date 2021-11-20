module FRPQuartet
  ( P2P (nothing, (|&|))
  , constant
  , null
  , P2S (never, (|||))
  , empty
  , Static (..)
  , Ref (..)
  , ref
  , ReadEntity (..)
  , WriteEntity (..)
  , Stream (..)
  , stream
  , ReadStream (..)
  , WriteStream (..)
  , foo
  , bar
  , FRPQuartet.readIO
  ) where

import           Data.Functor.Contravariant (Contravariant(..), (>$<))
import           Data.Void                  (Void, absurd)
import Data.IORef (newIORef, writeIORef, readIORef, modifyIORef)
import Control.Concurrent (putMVar, newEmptyMVar, takeMVar, forkIO)
import Control.Monad (forever, void)
import Data.Traversable (for)
import Data.Foldable (for_)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Control.Monad.Writer (WriterT (runWriterT, WriterT))
import Prelude hiding (null)

-- | Notice no @Functor f@ nor @Contravariant f@ constraint
-- laws:
-- u |&| nothing ~= u
-- nothing |&| u ~= u
-- (u |&| w) |&| z ~= u |&| (w |&| z)
class P2P f where
  nothing :: f ()
  (|&|) :: f a -> f b -> f (a, b)
  infixr 1 |&|

constant :: (Functor f, P2P f) => a -> f a
constant a = a <$ nothing

-- notice: (this function is not exported, it's just for documentation)
_constantReadEntity :: a -> ReadEntity a
_constantReadEntity = constant

null :: (Contravariant f, P2P f) => f a
null = () >$ nothing

-- notice: (this function is not exported, it's just for documentation)
_nullWriteEntity :: WriteEntity a
_nullWriteEntity = null

-- notice: (this function is not exported, it's just for documentation)
_nullWriteStream :: WriteStream a
_nullWriteStream = null

-- | Notice no @Functor f@ nor @Contravariant f@ constraint
-- | laws:
-- | u ||| never ~= u
-- | never ||| u ~= u
-- | (u ||| w) ||| z ~= u ||| (w ||| z)
class P2S f where
  never :: f Void
  (|||) :: f a -> f b -> f (Either a b)
  infixr 1 |||

empty :: (Functor f, P2S f) => f a
empty = absurd <$> never

-- notice: (this function is not exported, it's just for documentation)
_emptyReadStream :: ReadStream a
_emptyReadStream = empty

-- notice: (this function is not exported, it's just for documentation)
-- you cannot create ad-hoc arbitrary contravariant P2S
-- if you need to create ad-hoc arbitrary contravariant you have to use P2P and @null@
_nullP2S :: (Contravariant f, P2S f) => (a -> Void) -> f a
_nullP2S f = f >$< never

newtype Static f p a = Static { runStatic :: f (p a) }

instance (Functor p, Applicative f) => Functor (Static f p) where
  fmap f s = Static $ fmap f <$> runStatic s

instance (Contravariant p, Applicative f) => Contravariant (Static f p) where
  contramap f s = Static $ contramap f <$> runStatic s

instance (P2P p, Applicative f) => P2P (Static f p) where
  nothing = Static $ pure nothing
  sa |&| sb = Static $ (|&|) <$> runStatic sa <*> runStatic sb

instance (P2S p, Applicative f) => P2S (Static f p) where
  never = Static $ pure never
  sa ||| sb = Static $ (|||) <$> runStatic sa <*> runStatic sb


-- | Ref instantiates P2P
-- i.e. |&| :: Ref a -> Ref b -> Ref (a, b)
-- Ref does not instantiate Functor nor Contravariant
-- Ref could instantiate Invariant (left to prove)
data Ref a = Ref
  { writeRef :: Static (WriterT [String] Identity) WriteEntity a  -- contravariant, product to product
  , readRef  :: Static (WriterT [String] Identity) ReadEntity a   -- covariant, product to product
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

-- | ReadEntity instantiates P2P and additionally Functor
newtype ReadEntity a = ReadEntity { runReadEntity :: IO a }

instance P2P ReadEntity where
  nothing = ReadEntity $ return ()
  oea |&| oeb = ReadEntity $ (,) <$> runReadEntity oea <*> runReadEntity oeb

instance Functor ReadEntity where
  fmap f oe = ReadEntity $ f <$> runReadEntity oe

-- | ReadEntity instantiates P2P and additionally Contravariant
newtype WriteEntity a = WriteEntity { runWriteEntity :: a -> IO () }

instance P2S WriteEntity where
  never = WriteEntity $ \_ -> return ()
  ea ||| eb = WriteEntity $ \aorb -> either (runWriteEntity ea) (runWriteEntity eb) aorb

instance P2P WriteEntity where
  nothing = WriteEntity $ \_ -> return ()
  iea |&| ieb = WriteEntity $ \(a, b) -> do
    runWriteEntity iea a
    runWriteEntity ieb b


instance Contravariant WriteEntity where
  contramap f ie = WriteEntity $ runWriteEntity ie . f

-- | Stream instatiates P2S
-- i.e. ||| :: Stream a -> Stream b -> Stream (Either a b)
-- Stream does not instantiate Functor nor Contravariant
-- Stream could instantiate Invariant (left to prove)
data Stream a = Stream
  { writeStream :: WriteStream a
  , readStream :: ReadStream a
  }

instance P2S Stream where
  never = Stream
    { writeStream = never
    , readStream = never
    }
  ea ||| eb = Stream
    { writeStream = writeStream ea ||| writeStream eb
    , readStream = readStream ea ||| readStream eb
    }

-- | ReadStream instantiates P2S and additionally Functor
newtype ReadStream a = ReadStream { runReadStream :: (a -> IO ()) -> IO () }

instance P2S ReadStream where
  never = ReadStream $ \_ -> return ()
  osa ||| osb = ReadStream $ \aorb2io -> do
    runReadStream osa $ aorb2io . Left
    runReadStream osb $ aorb2io . Right

instance Functor ReadStream where
  fmap f os = ReadStream $ \b2io -> runReadStream os (b2io . f)

-- | WriteStream instantiates P2S and additionally Contravariant
newtype WriteStream a = WriteStream { runWriteStream :: a -> IO () }

instance P2S WriteStream where
  never = WriteStream $ \_ -> return ()
  sa ||| sb = WriteStream $ \aorb -> either (runWriteStream sa) (runWriteStream sb) aorb

instance P2P WriteStream where
  nothing = WriteStream $ \_ -> return ()
  isa |&| isb = WriteStream $ \(a, b) -> do
    runWriteStream isa a
    runWriteStream isb b

instance Contravariant WriteStream where
  contramap f is = WriteStream $ runWriteStream is . f

--

ref :: String -> a -> IO (Ref a)
ref name a = do
  ref <- newIORef a
  return $ Ref
    { writeRef = Static $ WriterT $ Identity (WriteEntity $ writeIORef ref, [name])
    , readRef = Static $ WriterT $ Identity (ReadEntity $ readIORef ref, [name])
    }

type ReadIO a = IO a

readIO :: String -> ReadIO a -> Static (WriterT [String] Identity) ReadEntity a
readIO name ioa = Static $ WriterT $ Identity (ReadEntity ioa, [name])

stream :: IO (Stream a)
stream = do
  mvarsRef <- newIORef []
  return $ Stream
    { writeStream = WriteStream $ \a -> do
        mvars <- readIORef mvarsRef
        for_ mvars $ \mvar -> putMVar mvar a
    , readStream = ReadStream $ \action -> do
        mvar <- newEmptyMVar
        modifyIORef mvarsRef (mvar:)
        void $ forkIO $ forever $ takeMVar mvar >>= action
    }

foo :: Static (WriterT [String] Identity) WriteEntity a -> a -> IO ()
foo siea a = let (doWriteEntity, meta) = (runIdentity . runWriterT . runStatic) siea
                in do
                  print meta
                  runWriteEntity doWriteEntity a

bar :: Static (WriterT [String] Identity) ReadEntity a -> IO a
bar siea = let (doReadEntity, meta) = (runIdentity . runWriterT . runStatic) siea
            in do
              print meta
              runReadEntity doReadEntity
