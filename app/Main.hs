module Main where

import FRPQuartet
import Data.Char (toUpper)
import Data.Functor.Contravariant
import Control.Concurrent (forkIO, threadDelay)

main :: IO ()
main = do
  firstName <- mkEntity "John"
  lastName <- mkEntity "Doe"
  let fullName = p2pCompose (firstName, lastName)

  -- write primitive entity
  runWriteEntity (writeEntity firstName) "Sam"
  -- write complex entity
  runWriteEntity (writeEntity fullName) ("Paul", "Smith")
  -- contramap writing entity
  runWriteEntity (fmap toUpper >$< writeEntity lastName) "newman"
  -- compose writing entity
  runWriteEntity (p2pCompose (writeEntity firstName, writeEntity lastName)) ("Henry", "Ford")

  -- read primitive entity
  runReadEntity (readEntity lastName) >>= print
  -- read complex entity
  runReadEntity (readEntity fullName) >>= print
  -- fmap reading entity
  runReadEntity (take 3 <$> readEntity lastName) >>= print
  -- compose reading entity
  runReadEntity (p2pCompose (readEntity firstName, readEntity lastName)) >>= print

  messages <- mkStream
  temperatures <- mkStream
  let notifications = p2sCompose (messages, temperatures)

  -- read primitive stream
  runReadStream (readStream messages) putStrLn
  runReadStream (readStream temperatures) print
  -- read complex stream
  runReadStream (readStream notifications) print
  -- fmap reading stream
  runReadStream ((<> "!") <$> readStream messages) putStrLn

  -- write primitive stream
  getLine; runWriteStream (writeStream messages) "Hello"
  getLine; runWriteStream (writeStream temperatures) 23
  -- write complex stream
  getLine; runWriteStream (writeStream notifications) (Left "World")
  -- contramap writing stream
  getLine; runWriteStream ((<> "!!!") >$< writeStream messages) "Greetings"

  -- wait for propagation
  threadDelay 1000000
  return ()
