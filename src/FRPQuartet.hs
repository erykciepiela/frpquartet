module FRPQuartet where

import           Data.Functor.Contravariant (Contravariant(..))
import           Data.Void                  (Void)
import Data.IORef (newIORef, writeIORef, readIORef)
import Control.Concurrent (putMVar, newEmptyMVar, takeMVar)
import Control.Monad (forever)

-- | Notice no @Functor f@ nor @Contravariant f@ constraint
-- laws:
-- p2pCompose (u, p2pUnit) ~= u
-- p2pCompose (p2pUnit, u) ~= u
-- p2pCompose (u, p2pCompose (w, z)) ~= p2pCompose (p2pCompose (u, w), z)
class ProductToProduct f where
  p2pUnit :: f ()
  p2pCompose :: (f a, f b) -> f (a, b)

-- | Entity instantiates ProductToProduct
-- i.e. p2pCompose :: (Entity a, Entity b) -> Entity (a, b)
-- Entity does not instantiate Functor nor Contravariant
-- Entity could instantiate Invariant (left to prove)
data Entity a = Entity
  { writeEntity :: WriteEntity a  -- contravariant, product to product
  , readEntity  :: ReadEntity a   -- covariant, product to product
  }

instance ProductToProduct Entity where
  p2pUnit = Entity
    { writeEntity = p2pUnit
    , readEntity = p2pUnit
    }
  p2pCompose (ea, eb) = Entity
    { writeEntity = p2pCompose (writeEntity ea, writeEntity eb)
    , readEntity = p2pCompose (readEntity ea, readEntity eb)
    }

-- | ReadEntity instantiates ProductToProduct and additionally Functor
newtype ReadEntity a = ReadEntity { runReadEntity :: IO a }

instance ProductToProduct ReadEntity where
  p2pUnit = ReadEntity $ return ()
  p2pCompose (oea, oeb) = ReadEntity $ (,) <$> runReadEntity oea <*> runReadEntity oeb

instance Functor ReadEntity where
  fmap f oe = ReadEntity $ f <$> runReadEntity oe

-- | ReadEntity instantiates ProductToProduct and additionally Contravariant
newtype WriteEntity a = WriteEntity { runWriteEntity :: a -> IO () }

instance ProductToProduct WriteEntity where
  p2pUnit = WriteEntity $ \_ -> return ()
  p2pCompose (iea, ieb) = WriteEntity $ \(a, b) -> do
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
  p2sCompose :: (f a, f b) -> f (Either a b)

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
  p2sCompose (ea, eb) = Stream
    { writeStream = p2sCompose (writeStream ea, writeStream eb)
    , readStream = p2sCompose (readStream ea, readStream eb)
    }

-- | ReadStream instantiates ProductToSum and additionally Functor
newtype ReadStream a = ReadStream { runReadStream :: (a -> IO ()) -> IO () }

instance ProductToSum ReadStream where
  p2sUnit = ReadStream $ \_ -> return ()
  p2sCompose (osa, osb) = ReadStream $ \aorb2io -> do
    runReadStream osa $ aorb2io . Left
    runReadStream osb $ aorb2io . Right

instance Functor ReadStream where
  fmap f os = ReadStream $ \b2io -> runReadStream os (b2io . f)

-- | WriteStream instantiates ProductToSum and additionally Contravariant
newtype WriteStream a = WriteStream { runWriteStream :: a -> IO () }

instance ProductToSum WriteStream where
  p2sUnit = WriteStream $ \_ -> return ()
  p2sCompose (isa, isb) = WriteStream $ \aorb -> either (runWriteStream isa) (runWriteStream isb) aorb

instance Contravariant WriteStream where
  contramap f is = WriteStream $ runWriteStream is . f

changeEntity :: ReadStream a -> WriteEntity a -> IO ()
changeEntity = undefined

captureEntityChange :: ReadEntity a -> WriteStream a -> IO ()
captureEntityChange = undefined

--

mkEntity :: a -> IO (Entity a)
mkEntity a = do
  ref <- newIORef a
  return $ Entity
    { writeEntity = WriteEntity $ writeIORef ref
    , readEntity = ReadEntity $ readIORef ref
    }

mkStream :: IO (Stream a)
mkStream = do
  mvar <- newEmptyMVar
  return $ Stream
    { writeStream = WriteStream $ putMVar mvar
    , readStream = ReadStream $ \action -> forever $ takeMVar mvar >>= action
    }
