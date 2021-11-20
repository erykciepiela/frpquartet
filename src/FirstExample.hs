{-# LANGUAGE TypeApplications #-}
module FirstExample where

import FRPQuartet
import Data.Functor.Contravariant
import Control.Concurrent
import Control.Monad.Identity (Identity(runIdentity))
import Data.Time (getCurrentTime)
import Prelude hiding (read, null, readIO)

main :: IO ()
main = do
  firstName <- ref "first name" "John"
  middleName <- ref "middle name" "Ferguson"
  lastName <- ref "last name" "Doe"
  let fullName = firstName |&| middleName |&| lastName
  pressure <- topic "pressure"
  temperature <- topic "temperature"
  wind <- topic "wind"
  let wheatherInfo = pressure ||| temperature ||| wind

  -- write primitive ref
  write (writeRef firstName) "Sam"
  -- write complex ref
  write (writeRef fullName) ("Paul", ("Adam", "Smith"))
  -- contramap writing ref
  write (reverse >$< writeRef lastName) "namweN"
  -- compose writing all refs
  write (writeRef firstName |&| writeRef lastName) ("Henry", "Ford")
  -- compose writing some ref
  write (writeRef firstName ||| writeRef lastName) (Right "Ford!")
  -- write to null entity
  write null "abc"

  -- read primitive ref
  read (readRef lastName) >>= print
  -- read complex ref
  read (readRef fullName) >>= print
  -- fmap reading ref
  read (take 3 <$> readRef lastName) >>= print
  -- compose reading all refs
  read (readRef firstName |&| readRef lastName) >>= print
  -- read from IO
  read (readIO "current time" getCurrentTime) >>= print
  -- read constant
  read (constant 5) >>= print

  -- read primitive topic
  subscribe (readTopic pressure) print
  subscribe (readTopic temperature) print
  -- read complex topic
  subscribe (readTopic wheatherInfo) print
  -- fmap reading topic
  subscribe ((+ 10) <$> readTopic pressure) print
  -- compose reading streams
  subscribe (readTopic pressure ||| readTopic temperature) print
  -- read empty topic
  subscribe empty putStrLn


  -- write primitive topic
  getLine; write (writeTopic pressure) 1001
  getLine; write (writeTopic temperature) 23.5
  getLine; write (writeTopic wind) 2.3
  -- write complex topic
  getLine; write (writeTopic wheatherInfo) (Right (Left 23.8))
  -- contramap writing topic
  getLine; write ((+ 2) >$< writeTopic pressure) 999
  -- compose writing some topic
  getLine; write (writeTopic pressure ||| writeTopic temperature) (Right 19)
  -- compose writing all streams
  getLine; write (writeTopic pressure |&| writeTopic temperature) (1021, 21)
  -- write to null topic
  getLine; write null 17
  -- filter topic
  getLine; write (null ||| writeTopic temperature) (Right 19)
  -- filter topic
  getLine; write (writeTopic temperature ||| writeRef lastName) (Left 19)
  -- write to topic or to ref
  getLine; write (writeTopic temperature ||| writeRef firstName) (Left 19)
  -- write to topic and to ref
  getLine; write (writeTopic temperature |&| writeRef firstName) (19, "Ian")

  -- wait for propagation
  threadDelay 1000000
  return ()
