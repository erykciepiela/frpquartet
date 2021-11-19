module Main where

import FRPQuartet
import Data.Char (toUpper)
import Data.Functor.Contravariant
import Control.Concurrent (forkIO, threadDelay)

main :: IO ()
main = do
  firstName <- mkEntity "John"
  lastName <- mkEntity "Doe"
  let name = p2pCompose (firstName, lastName)

  -- write primitive entity
  runWriteEntity (writeEntity firstName) "Sam"
  -- write complex entity
  runWriteEntity (writeEntity name) ("Paul", "Smith")
  -- contramap writing entity
  runWriteEntity (fmap toUpper >$< writeEntity lastName) "newman"
  -- compose writing entity
  runWriteEntity (p2pCompose (writeEntity firstName, writeEntity lastName)) ("Henry", "Ford")

  -- read primitive entity
  runReadEntity (readEntity lastName) >>= print
  -- read complex entity
  runReadEntity (readEntity name) >>= print
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

  getLine
  -- write primitive stream
  runWriteStream (writeStream messages) "Hello"
  getLine
  -- write primitive stream
  runWriteStream (writeStream temperatures) 23
  getLine
  -- write complex stream
  runWriteStream (writeStream notifications) (Left "World")
  getLine
  -- contramap writing stream
  runWriteStream ((<> "!!!") >$< writeStream messages) "Greetings"

  threadDelay 1000000
  return ()
