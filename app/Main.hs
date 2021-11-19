module Main where

import FRPQuartet
import Data.Char (toUpper)
import Data.Functor.Contravariant

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

  -- TODO example with of streams

  return ()
