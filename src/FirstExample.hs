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

data Person = Person
  { firstName :: FirstName
  , lastName :: LastName
  , location :: Location }
  deriving Show

type FirstName = String

type LastName = String

data Location
  = Address String
  | Coords String
  deriving Show

data WheatherInfo
  = Pressure Int
  | Temperature Float
  | Wind Float
  deriving Show

main :: IO ()
main = app person text wheatherInfo
  where
    person = ref @Person "person"
    text = ref @String "text"
    wheatherInfo = topic @WheatherInfo "wheather info"

app :: Static IO Ref Person -> Static IO Ref String -> Static IO Topic WheatherInfo -> IO ()
app person' text' wheatherInfo' = do
  person <- runStatic person'
  wheatherInfo <- runStatic wheatherInfo'

  let writePerson = writeRef person
  writeEntity writePerson $ Person { firstName = "Paul", lastName = "Smith", location = Coords "50N20E" }

  let writeReversedFirstName = undefined >$< writePerson -- TODO

  let readPerson = readRef person
  testRead "Person" readPerson

  let readFirstName = firstName <$> readPerson
  testRead "First Name" readFirstName

  let readLastName = lastName <$> readPerson
  testRead "Last Name" readLastName

  let readFirstAndLastName = (,) <$> readFirstName <*> readLastName
  testRead "First and Last Name" readFirstAndLastName

  -- let readAddress = fst $ expand $ thrd <$> readPerson
  -- testRead "Address" readAddress

  -- let readCoords = snd $ expand $ thrd <$> readPerson
  -- testRead "Coords" readCoords

  let readFive = constant 5
  testRead "Five" readFive

  let readCurrentTime = readIO "current time" getCurrentTime
  testRead "Current Time" readCurrentTime

  --
  let subscribeWheatherInfo = subscribeTopic wheatherInfo
  subscribe subscribeWheatherInfo print

  -- let subscribePressure = fst $ expand subscribeWheatherInfo
  let subscribePressure = undefined :: FRP SubscribeStream Int
  subscribe subscribePressure print

  -- let subscribeTemperature = snd $ expand subscribeWheatherInfo
  let subscribeTemperature = undefined :: FRP SubscribeStream Float
  subscribe subscribeTemperature print

  let subscribeAdjustedPressure = (+ 10) <$> subscribePressure
  subscribe subscribeAdjustedPressure print

  let subscribePressureAndTemperature = subscribePressure ||| subscribeTemperature
  subscribe subscribePressureAndTemperature print

  let subscribeEmpty = empty @Int
  subscribe subscribeEmpty print

  let writeWhetherInfo = writeTopic wheatherInfo

  -- let writePressure = fst $ expand writeWhetherInfo
  let writePressure = undefined :: FRP WriteStream Int
  getLine; writeStream writePressure 1002

  let writeAdjustedPressure = (+ 10) >$< writePressure
  getLine; writeStream writePressure 1002

  -- let writeTemeprature = fst $ expand $ snd $ expand writeWhetherInfo
  let writeTemeprature = undefined
  getLine; writeStream writeTemeprature 23.8

  -- let writeWind = snd $ expand $ snd $ expand writeWhetherInfo
  let writeWind = snd undefined
  getLine; writeStream writeWind 23.8

  -- wait for propagation
  threadDelay 1000000
  return ()
