{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

type FirstName = String
type LastName = String
type Address = String
type Coords = String
type Temperature = Float
type Pressure = Int
type Wind = Float

thrd :: (a, (b, c)) -> c
thrd (_, (_, a)) = a

snd' :: (a, (b, c)) -> b
snd' (_, (b, _)) = b

main :: IO ()
main = do
  firstName <- ref @FirstName "first name"
  lastName <- ref @LastName "last name"
  location <- ref @(Either Address Coords) "location"

  pressure <- topic @Pressure "pressure"
  temperature <- topic @Temperature "temperature"
  wind <- topic @Wind "wind"

  let writePerson = writeRef $ firstName |&| lastName |&| location
  getLine; writeEntity writePerson $ Just ("Paul", ("Smith", Right "50N20E"))

  let writeReversedFirstName = undefined >$< writePerson -- TODO

  let writeNull = null
  getLine; writeEntity writeNull $ Just 12

  let readPerson = readRef $ firstName |&| lastName |&| location
  getLine; readEntity readPerson >>= print

  let readFirstName = readRef firstName
  getLine; readEntity readFirstName >>= print

  let readLastName = readRef lastName
  getLine; readEntity readLastName >>= print

  let readFirstAndLastName = (,) <$> readFirstName <*> readLastName
  getLine; readEntity readFirstAndLastName >>= print

  let readAddress = fst $ expand $ readRef location
  getLine; readEntity readAddress >>= print

  let readCoords = snd $ expand $ readRef location
  getLine; readEntity readCoords >>= print

  let readConstant = constant 5
  getLine; readEntity readConstant >>= print

  let readCurrentTime = readIO "current time" getCurrentTime
  getLine; readEntity readCurrentTime >>= print

  ---

  foo <- topic @(Int, Bool) "whatever"
  let fooSwapped = invmap swap swap foo

  tuple <- ref "tuple"
  let tupleSwapped = invmap swap swap tuple

  --
  let subscribeWheatherInfo = subscribeTopic $ pressure ||| temperature ||| wind
  subscribeStream subscribeWheatherInfo print

  let subscribePressure = subscribeTopic pressure
  subscribeStream subscribePressure print

  let subscribeTemperature = subscribeTopic temperature
  subscribeStream subscribeTemperature print

  let subscribeAdjustedPressure = (+ 10) <$> subscribePressure
  subscribeStream subscribeAdjustedPressure print

  let subscribePressureAndTemperature = subscribePressure ||| subscribeTemperature
  subscribeStream subscribePressureAndTemperature print

  let subscribeEmpty = empty @Int
  subscribeStream subscribeEmpty print

  let writeWhetherInfo = writeTopic $ pressure ||| temperature ||| wind

  let writePressure = fst $ expand writeWhetherInfo
  getLine; writeStream writePressure 1002

  let writeAdjustedPressure = (+ 10) >$< writePressure
  getLine; writeStream writePressure 1002

  let writeTemeprature = fst $ expand $ snd $ expand writeWhetherInfo
  getLine; writeStream writeTemeprature 23.8

  let writeWind = snd $ expand $ snd $ expand writeWhetherInfo
  getLine; writeStream writeWind 23.8

  -- wait for propagation
  threadDelay 1000000
  return ()
