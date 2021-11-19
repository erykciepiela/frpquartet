module FirstExample where

import FRPQuartet
import Data.Char (toUpper)
import Data.Functor.Contravariant
import Control.Concurrent (forkIO, threadDelay)

main :: IO ()
main = do
  firstName <- entity "John"
  middleName <- entity "Ferguson"
  lastName <- entity "Doe"
  let fullName = firstName |&| middleName |&| lastName

  -- write primitive entity
  runWriteEntity (writeEntity firstName) "Sam"
  -- write complex entity
  runWriteEntity (writeEntity fullName) ("Paul", ("Adam", "Smith"))
  -- contramap writing entity
  runWriteEntity (reverse >$< writeEntity lastName) "namweN"
  -- compose writing entity
  runWriteEntity (writeEntity firstName |&| writeEntity lastName) ("Henry", "Ford")

  -- read primitive entity
  runReadEntity (readEntity lastName) >>= print
  -- read complex entity
  runReadEntity (readEntity fullName) >>= print
  -- fmap reading entity
  runReadEntity (take 3 <$> readEntity lastName) >>= print
  -- compose reading entity
  runReadEntity (readEntity firstName |&| readEntity lastName) >>= print

  pressure <- stream
  temperature <- stream
  wind <- stream
  let wheatherInfo = pressure ||| temperature ||| wind

  -- read primitive stream
  runReadStream (readStream pressure) print
  runReadStream (readStream temperature) print
  -- read complex stream
  runReadStream (readStream wheatherInfo) print
  -- fmap reading stream
  runReadStream ((+ 10) <$> readStream pressure) print
  -- compose reading stream
  runReadStream (readStream pressure ||| readStream temperature) print

  -- write primitive stream
  getLine; runWriteStream (writeStream pressure) 1001
  getLine; runWriteStream (writeStream temperature) 23.5
  getLine; runWriteStream (writeStream wind) 2.3
  -- write complex stream
  getLine; runWriteStream (writeStream wheatherInfo) (Right (Left 23.8))
  -- contramap writing stream
  getLine; runWriteStream ((+ 2) >$< writeStream pressure) 999
  -- compose writing stream
  getLine; runWriteStream (writeStream pressure ||| writeStream temperature) (Right 19)

  -- wait for propagation
  threadDelay 1000000
  return ()
