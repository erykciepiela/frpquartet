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
  -- compose writing entities
  foo (writeRef firstName |&| writeRef lastName) ("Henry", "Ford")
  -- compose writing entities
  foo (writeRef firstName ||| writeRef lastName) (Right "Ford!")
  -- write to null
  foo null "abc"

  -- read primitive entity
  bar (readRef lastName) >>= print
  -- read complex entity
  bar (readRef fullName) >>= print
  -- fmap reading entity
  bar (take 3 <$> readRef lastName) >>= print
  -- compose reading entities
  bar (readRef firstName |&| readRef lastName) >>= print
  -- read from IO
  bar (readIO "current time" getCurrentTime) >>= print
  -- read constants
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

  -- write primitive stream
  getLine; runWriteStream (writeStream pressure) 1001
  getLine; runWriteStream (writeStream temperature) 23.5
  getLine; runWriteStream (writeStream wind) 2.3
  -- write complex stream
  getLine; runWriteStream (writeStream wheatherInfo) (Right (Left 23.8))
  -- contramap writing stream
  getLine; runWriteStream ((+ 2) >$< writeStream pressure) 999
  -- compose writing streams
  getLine; runWriteStream (writeStream pressure ||| writeStream temperature) (Right 19)

  -- wait for propagation
  threadDelay 1000000
  return ()
