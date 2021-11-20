module FirstExample where

import FRPQuartet
import Data.Functor.Contravariant
import Control.Concurrent
import Control.Monad.Identity (Identity(runIdentity))
import Data.Time (getCurrentTime)
import Prelude hiding (readIO)

main :: IO ()
main = do
  firstName <- entity "first name" "John"
  middleName <- entity "middle name" "Ferguson"
  lastName <- entity "last name" "Doe"
  let fullName = firstName |&| middleName |&| lastName
  pressure <- stream
  temperature <- stream
  wind <- stream
  let wheatherInfo = pressure ||| temperature ||| wind

  -- write primitive entity
  foo (writeEntity firstName) "Sam"
  -- write complex entity
  foo (writeEntity fullName) ("Paul", ("Adam", "Smith"))
  -- contramap writing entity
  foo (reverse >$< writeEntity lastName) "namweN"
  -- compose writing entities
  foo (writeEntity firstName |&| writeEntity lastName) ("Henry", "Ford")
  -- compose writing entities
  foo (writeEntity firstName ||| writeEntity lastName) (Right "Ford!")

  -- read primitive entity
  bar (readEntity lastName) >>= print
  -- read complex entity
  bar (readEntity fullName) >>= print
  -- fmap reading entity
  bar (take 3 <$> readEntity lastName) >>= print
  -- compose reading entities
  bar (readEntity firstName |&| readEntity lastName) >>= print
  -- read from IO and constants
  bar (readIO "current time" getCurrentTime |&| constant "five" 5) >>= print

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
