{-# LANGUAGE TypeApplications #-}
module FirstExample where

import FRPQuartet
import Data.Functor.Contravariant
import Control.Concurrent
import Control.Monad.Identity (Identity(runIdentity))
import Data.Time (getCurrentTime)
import Prelude hiding (null, readIO)

main :: IO ()
main = do
  firstName <- ref "first name" "John"
  middleName <- ref "middle name" "Ferguson"
  lastName <- ref "last name" "Doe"
  let fullName = firstName |&| middleName |&| lastName
  pressure <- stream
  temperature <- stream
  wind <- stream
  let wheatherInfo = pressure ||| temperature ||| wind

  -- write primitive ref
  foo (writeRef firstName) "Sam"
  -- write complex ref
  foo (writeRef fullName) ("Paul", ("Adam", "Smith"))
  -- contramap writing ref
  foo (reverse >$< writeRef lastName) "namweN"
  -- compose writing all refs
  foo (writeRef firstName |&| writeRef lastName) ("Henry", "Ford")
  -- compose writing some ref
  foo (writeRef firstName ||| writeRef lastName) (Right "Ford!")
  -- write to null entity
  foo null "abc"

  -- read primitive ref
  bar (readRef lastName) >>= print
  -- read complex ref
  bar (readRef fullName) >>= print
  -- fmap reading ref
  bar (take 3 <$> readRef lastName) >>= print
  -- compose reading all refs
  bar (readRef firstName |&| readRef lastName) >>= print
  -- read from IO
  bar (readIO "current time" getCurrentTime) >>= print
  -- read constant
  bar (constant 5) >>= print

  -- read primitive stream
  runReadStream (readStream pressure) print
  runReadStream (readStream temperature) print
  -- read complex stream
  runReadStream (readStream wheatherInfo) print
  -- fmap reading stream
  runReadStream ((+ 10) <$> readStream pressure) print
  -- compose reading streams
  runReadStream (readStream pressure ||| readStream temperature) print
  -- read empty stream
  runReadStream empty putStrLn


  -- write primitive stream
  getLine; runWrite (writeStream pressure) 1001
  getLine; runWrite (writeStream temperature) 23.5
  getLine; runWrite (writeStream wind) 2.3
  -- write complex stream
  getLine; runWrite (writeStream wheatherInfo) (Right (Left 23.8))
  -- contramap writing stream
  getLine; runWrite ((+ 2) >$< writeStream pressure) 999
  -- compose writing some stream
  getLine; runWrite (writeStream pressure ||| writeStream temperature) (Right 19)
  -- compose writing all streams
  getLine; runWrite (writeStream pressure |&| writeStream temperature) (1021, 21)
  -- write to null stream
  getLine; runWrite null 17
  -- filter stream
  getLine; runWrite (null ||| writeStream temperature) (Right 19)

  -- wait for propagation
  threadDelay 1000000
  return ()
