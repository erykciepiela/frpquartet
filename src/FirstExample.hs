{-# LANGUAGE TypeApplications #-}
module FirstExample where

import Quartet
import FRP
import Data.Functor.Contravariant
import Control.Concurrent
import Control.Monad.Identity (Identity(runIdentity))
import Data.Time (getCurrentTime)
import Prelude hiding (read, null, readIO)
import Data.Functor.Invariant (Invariant(invmap))
import Data.Tuple (swap)

main :: IO ()
main = do
  -- refs
  firstName <- ref "first name"
  middleName <- ref "middle name"
  lastName <- ref "last name"
  tuple <- ref "tuple"
  let tupleSwapped = invmap swap swap tuple
  let fullName = firstName |&| middleName |&| lastName

  -- writes

  -- write primitive ref
  let writeFirstName = writeRef firstName
  let writeLastName = writeRef lastName
  -- write complex ref
  let writeFullName = writeRef fullName
  -- contramap writing ref
  let writeReversedFirstName = reverse >$< writeFirstName
  -- writing to multiple refs
  let writeFirstAndLastName = writeFirstName |&| writeLastName
  -- write to no ref
  let writeNull = null
  -- -- write to chosen ref
  -- let writeFirstOrLastName = writeFirstName ||| writeLastName
  -- write to write that cannot be written
  let writeUnwritable = foo

  pressure <- topic "pressure"
  temperature <- topic "temperature"
  wind <- topic "wind"
  foo <- topic @(Int, Bool) "whatever"
  let fooSwapped = invmap swap swap foo
  let wheatherInfo = pressure ||| temperature ||| wind

  writeEntity writeFirstName "Sam"
  writeEntity writeFullName ("Paul", ("Adam", "Smith"))
  writeEntity writeReversedFirstName "namweN"
  writeEntity writeFirstAndLastName ("Henry", "Ford")
  writeEntity writeNull "abc"
  -- writeEntity writeFirstOrLastName (Right "Ford!")
  writeEntity writeUnwritable undefined

  -- read primitive ref
  readEntity (readRef lastName) >>= print
  -- read complex ref
  readEntity (readRef fullName) >>= print
  -- fmap reading ref
  readEntity (take 3 <$> readRef lastName) >>= print
  -- compose reading all refs
  readEntity (readRef firstName |&| readRef lastName) >>= print
  -- read from IO
  readEntity (readIO "current time" getCurrentTime) >>= print
  -- read constant
  readEntity (constant 5) >>= print

  -- read primitive topic
  subscribe (subscribeTopic pressure) print
  subscribe (subscribeTopic temperature) print
  -- read complex topic
  subscribe (subscribeTopic wheatherInfo) print
  -- fmap reading topic
  subscribe ((+ 10) <$> subscribeTopic pressure) print
  -- compose reading streams
  subscribe (subscribeTopic pressure ||| subscribeTopic temperature) print
  -- read empty topic
  subscribe empty putStrLn


  -- write primitive topic
  getLine; writeStream (writeTopic pressure) 1001
  getLine; writeStream (writeTopic temperature) 23.5
  getLine; writeStream (writeTopic wind) 2.3
  -- write complex topic
  getLine; writeStream (writeTopic wheatherInfo) (Right (Left 23.8))
  -- contramap writing topic
  getLine; writeStream ((+ 2) >$< writeTopic pressure) 999
  -- compose writing some topic
  getLine; writeStream (writeTopic pressure ||| writeTopic temperature) (Right 19)
  -- -- compose writing all streams
  -- getLine; writeStream (writeTopic pressure |&| writeTopic temperature) (1021, 21)
  -- write to null topic
  -- getLine; writeStream null 17
  -- filter topic
  -- getLine; writeStream (null ||| writeTopic temperature) (Right 19)
  -- -- filter topic
  -- getLine; writeStream (writeTopic temperature ||| writeRef lastName) (Left 19)
  -- -- write to topic or to ref
  -- getLine; writeStream (writeTopic temperature ||| writeRef firstName) (Left 19)
  -- -- write to topic and to ref
  -- getLine; writeStream (writeTopic temperature |&| writeRef firstName) (19, "Ian")

  -- wait for propagation
  threadDelay 1000000
  return ()
