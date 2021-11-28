{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FirstExample where

import Quartet
import FRP
import Data.Functor.Contravariant
import Control.Concurrent
import Control.Monad.Identity (Identity(runIdentity), join)
import Data.Time (getCurrentTime)
import Prelude hiding (read, null, readIO)
import Data.Functor.Invariant (Invariant(invmap))
import Data.Tuple (swap)
import Control.Monad.State (StateT(runStateT))

type FirstName = String
type LastName = String
type Address = String
type Coords = String
type Temperature = Float
type Pressure = Int
type Wind = Float


firstName :: FRP' Ref FirstName
firstName = ref "first name"

lastName :: FRP' Ref LastName
lastName = ref "last name"

location :: FRP' Ref (Either Address Coords)
location = ref "location"

pressure :: FRP' Topic Pressure
pressure = topic "pressure"

temperature :: FRP' Topic Temperature
temperature = topic "temperature"

wind :: FRP' Topic Wind
wind = topic "wind"

person :: FRP' Ref (FirstName, (LastName, Either Address Coords))
person = firstName |&| lastName |&| location

writePerson :: FRP' WriteEntity (FirstName, (LastName, Either Address Coords))
writePerson = writeRef' $ firstName |&| lastName |&| location

writeReversedFirstName :: FRP' WriteEntity a
writeReversedFirstName = undefined >$< writePerson -- TODO

readPerson :: FRP' ReadEntity (FirstName, (LastName, Either Address Coords))
readPerson = readRef' $ firstName |&| lastName |&| location

readFirstName :: FRP' ReadEntity FirstName
readFirstName = readRef' firstName

readLastName :: FRP' ReadEntity LastName
readLastName = readRef' lastName

readFirstAndLastName :: FRP' ReadEntity (FirstName, LastName)
readFirstAndLastName = (,) <$> readFirstName <*> readLastName

readAddress :: FRP' ReadEntity Address
readAddress = fst $ expand $ readRef' location

readCoords :: FRP' ReadEntity Coords
readCoords = snd $ expand $ readRef' location

readFive :: FRP' ReadEntity Int
readFive = constant 5

subscribeWheatherInfo :: FRP' SubscribeStream (Either Pressure (Either Temperature Wind))
subscribeWheatherInfo = subscribeTopic' $ pressure ||| temperature ||| wind

subscribePressure :: FRP' SubscribeStream Pressure
subscribePressure = subscribeTopic' pressure

subscribeTemperature :: FRP' SubscribeStream Temperature
subscribeTemperature = subscribeTopic' temperature

subscribeAdjustedPressure :: FRP' SubscribeStream Pressure
subscribeAdjustedPressure = (+ 10) <$> subscribePressure

subscribePressureAndTemperature :: FRP' SubscribeStream (Either Pressure Temperature)
subscribePressureAndTemperature = subscribePressure ||| subscribeTemperature

subscribeEmpty :: FRP' SubscribeStream Int
subscribeEmpty = empty @Int

writeWhetherInfo :: FRP' WriteStream (Either Pressure (Either Temperature Wind))
writeWhetherInfo = writeTopic' $ pressure ||| temperature ||| wind

writePressure :: FRP' WriteStream Pressure
writePressure = writeTopic' pressure

writeAdjustedPressure :: FRP' WriteStream Pressure
writeAdjustedPressure = (+ 10) >$< writePressure

writeTemeprature :: FRP' WriteStream Temperature
writeTemeprature = writeTopic' temperature

writeWind :: FRP' WriteStream Wind
writeWind = writeTopic' wind

main :: IO ()
main = do

  (writePerson, readPerson , readFirstName) <- foo $ do
    wp <- runStatic writePerson
    rp <- runStatic readPerson
    readFirstName <- runStatic readFirstName
    return (wp, rp, readFirstName)

  getLine; runWriteEntity writePerson $ Just ("Paul", ("Smith", Right "50N20E"))
  getLine; runReadEntity readFirstName >>= print
  -- getLine; runReadEntity readLastName >>= print
  -- getLine; runReadEntity readAddress >>= print
  -- getLine; runReadEntity readCoords >>= print

  -- subscribeStream subscribeWheatherInfo print
  -- subscribeStream subscribePressure print
  -- subscribeStream subscribeTemperature print
  -- subscribeStream subscribeAdjustedPressure print
  -- subscribeStream subscribePressureAndTemperature print

  -- getLine; writeStream writePressure 1002
  -- getLine; writeStream writePressure 1002
  -- getLine; writeStream writeTemeprature 23.8
  -- getLine; writeStream writeWind 23.8

  -- wait for propagation
  threadDelay 1000000
  return ()
