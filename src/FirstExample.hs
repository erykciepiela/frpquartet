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
  -- refs
  _firstName <- ref @FirstName "first name"
  _lastName <- ref @LastName "last name"
  _location <- ref @(Either Address Coords) "location"
  let person = _firstName |&| _lastName |&| _location

  -- topics
  _pressure <- topic @Pressure "pressure"
  _temperature <- topic @Temperature "temperature"
  _wind <- topic @Wind "wind"
  let wheatherInfo = _pressure ||| _temperature ||| _wind


  let writePerson = writeRef person
  getLine; writeEntity writePerson ("Paul", ("Smith", Right "50N20E"))

  let writeReversedFirstName = undefined >$< writePerson -- TODO

  let writeNull = null
  getLine; writeEntity writeNull 12

  let writeUnwritable = foo
  getLine; writeEntity writeUnwritable undefined

  let readPerson = readRef person
  getLine; readEntity readPerson >>= print

  let readFirstName = fst <$> readPerson
  getLine; readEntity readFirstName >>= print

  let readLastName = snd' <$> readPerson
  getLine; readEntity readLastName >>= print

  let readFirstAndLastName = (,) <$> readFirstName <*> readLastName
  getLine; readEntity readFirstAndLastName >>= print

  let readAddress = fst $ expand $ thrd <$> readPerson
  getLine; readEntity readAddress >>= print

  let readCoords = snd $ expand $ thrd <$> readPerson
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
  let subscribeWheatherInfo = subscribeTopic wheatherInfo
  subscribe subscribeWheatherInfo print

  let subscribePressure = fst $ expand subscribeWheatherInfo
  subscribe subscribePressure print

  let subscribeTemperature = snd $ expand subscribeWheatherInfo
  subscribe subscribeTemperature print

  let subscribeAdjustedPressure = (+ 10) <$> subscribePressure
  subscribe subscribeAdjustedPressure print

  let subscribePressureAndTemperature = subscribePressure ||| subscribeTemperature
  subscribe subscribePressureAndTemperature print

  let subscribeEmpty = empty @Int
  subscribe subscribeEmpty print

  let writeWhetherInfo = writeTopic wheatherInfo

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
