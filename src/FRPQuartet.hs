module FRPQuartet where

import           Data.Functor.Contravariant (Contravariant(..))
import           Data.Void                  (Void)
import Data.IORef (newIORef, writeIORef, readIORef, modifyIORef)
import Control.Concurrent (putMVar, newEmptyMVar, takeMVar, forkIO)
import Control.Monad (forever, void)
import Data.Traversable (for)
import Data.Foldable (for_)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Control.Monad.Writer (WriterT (runWriterT, WriterT))

-- | Notice no @Functor f@ nor @Contravariant f@ constraint
-- laws:
-- p2pCompose (u, p2pUnit) ~= u
-- p2pCompose (p2pUnit, u) ~= u
-- p2pCompose (u, p2pCompose (w, z)) ~= p2pCompose (p2pCompose (u, w), z)
class ProductToProduct f where
  p2pUnit :: f ()
  (|&|) :: f a -> f b -> f (a, b)
  infixr 1 |&|

newtype Static f p a = Static { runStatic :: f (p a) }

instance (Contravariant p, Applicative f) => Contravariant (Static f p) where
  contramap f s = Static $ contramap f <$> runStatic s

instance (Functor p, Applicative f) => Functor (Static f p) where
  fmap f s = Static $ fmap f <$> runStatic s

instance (ProductToProduct p, Applicative f) => ProductToProduct (Static f p) where
  p2pUnit = Static $ pure p2pUnit
  sa |&| sb = Static $ (|&|) <$> runStatic sa <*> runStatic sb
instance (ProductToSum p, Applicative f) => ProductToSum (Static f p) where
  p2sUnit = Static $ pure p2sUnit
  sa ||| sb = Static $ (|||) <$> runStatic sa <*> runStatic sb

-- | Entity instantiates ProductToProduct
-- i.e. p2pCompose :: (Entity a, Entity b) -> Entity (a, b)
-- Entity does not instantiate Functor nor Contravariant
-- Entity could instantiate Invariant (left to prove)
data Entity a = Entity
  { writeEntity :: Static (WriterT [String] Identity) WriteEntity a  -- contravariant, product to product
  , readEntity  :: Static (WriterT [String] Identity) ReadEntity a   -- covariant, product to product
  }

-- foo :: Static Identity Entity a -> WriteEntity a
-- foo = fst . runIdentity . runWriterT . runStatic . writeEntity . runIdentity . runStatic

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

instance ProductToProduct Entity where
  p2pUnit = Entity
    { writeEntity = p2pUnit
    , readEntity = p2pUnit
    }
  ea |&| eb = Entity
    { writeEntity = writeEntity ea |&| writeEntity eb
    , readEntity = readEntity ea |&| readEntity eb
    }

-- | ReadEntity instantiates ProductToProduct and additionally Functor
newtype ReadEntity a = ReadEntity { runReadEntity :: IO a }

instance ProductToProduct ReadEntity where
  p2pUnit = ReadEntity $ return ()
  oea |&| oeb = ReadEntity $ (,) <$> runReadEntity oea <*> runReadEntity oeb

instance Functor ReadEntity where
  fmap f oe = ReadEntity $ f <$> runReadEntity oe

-- | ReadEntity instantiates ProductToProduct and additionally Contravariant
newtype WriteEntity a = WriteEntity { runWriteEntity :: a -> IO () }

instance ProductToSum WriteEntity where
  p2sUnit = WriteEntity $ \_ -> return ()
  ea ||| eb = WriteEntity $ \aorb -> either (runWriteEntity ea) (runWriteEntity eb) aorb

instance ProductToProduct WriteEntity where
  p2pUnit = WriteEntity $ \_ -> return ()
  iea |&| ieb = WriteEntity $ \(a, b) -> do
    runWriteEntity iea a
    runWriteEntity ieb b


instance Contravariant WriteEntity where
  contramap f ie = WriteEntity $ runWriteEntity ie . f

-- | Notice no @Functor f@ nor @Contravariant f@ constraint
-- | laws:
-- | p2sCompose (u, p2sUnit) ~= u
-- | p2sCompose (p2sUnit, u) ~= u
-- | p2sCompose (u, p2sCompose (w, z)) ~= p2sCompose (p2sCompose (u, w), z)
class ProductToSum f where
  p2sUnit :: f Void
  -- p2sCompose :: (f a, f b) -> f (Either a b)
  (|||) :: f a -> f b -> f (Either a b)
  infixr 1 |||


-- | Stream instatiates ProductToSum
-- i.e. p2cCompose :: (Stream a, Stream b) -> Stream (Either a b)
-- Stream does not instantiate Functor nor Contravariant
-- Stream could instantiate Invariant (left to prove)
data Stream a = Stream
  { writeStream :: WriteStream a
  , readStream :: ReadStream a
  }

instance ProductToSum Stream where
  p2sUnit = Stream
    { writeStream = p2sUnit
    , readStream = p2sUnit
    }
  ea ||| eb = Stream
    { writeStream = writeStream ea ||| writeStream eb
    , readStream = readStream ea ||| readStream eb
    }

-- | ReadStream instantiates ProductToSum and additionally Functor
newtype ReadStream a = ReadStream { runReadStream :: (a -> IO ()) -> IO () }

instance ProductToSum ReadStream where
  p2sUnit = ReadStream $ \_ -> return ()
  osa ||| osb = ReadStream $ \aorb2io -> do
    runReadStream osa $ aorb2io . Left
    runReadStream osb $ aorb2io . Right

instance Functor ReadStream where
  fmap f os = ReadStream $ \b2io -> runReadStream os (b2io . f)

-- | WriteStream instantiates ProductToSum and additionally Contravariant
newtype WriteStream a = WriteStream { runWriteStream :: a -> IO () }

instance ProductToSum WriteStream where
  p2sUnit = WriteStream $ \_ -> return ()
  sa ||| sb = WriteStream $ \aorb -> either (runWriteStream sa) (runWriteStream sb) aorb

instance ProductToProduct WriteStream where
  p2pUnit = WriteStream $ \_ -> return ()
  isa |&| isb = WriteStream $ \(a, b) -> do
    runWriteStream isa a
    runWriteStream isb b

instance Contravariant WriteStream where
  contramap f is = WriteStream $ runWriteStream is . f

changeEntity :: ReadStream a -> WriteEntity a -> IO ()
changeEntity = undefined

captureEntityChange :: ReadEntity a -> WriteStream a -> IO ()
captureEntityChange = undefined

--

entity :: String -> a -> IO (Entity a)
entity name a = do
  ref <- newIORef a
  return $ Entity
    { writeEntity = Static $ WriterT $ Identity (WriteEntity $ writeIORef ref, [name])
    , readEntity = Static $ WriterT $ Identity (ReadEntity $ readIORef ref, [name])
    }

constant :: String -> a -> Static (WriterT [String] Identity) ReadEntity a
constant name a = Static $ WriterT $ Identity (ReadEntity $ return a, [name])

readIO :: String -> IO a -> Static (WriterT [String] Identity) ReadEntity a
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
