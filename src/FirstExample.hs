{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FirstExample where

import Quartet hiding ((|||), (|&|))
import FRP
import Data.Functor.Contravariant
import Control.Monad.IO.Class (MonadIO(liftIO))
import Actegory
import Data.Void (Void)

type FirstName = String
type LastName = String
type Address = String
type Coords = String
type Temperature = Float
type Pressure = Int
type Wind = Float

firstName :: FRP Ref FirstName
firstName = ref "first name"

lastName :: FRP Ref LastName
lastName = ref "last name"

location :: FRP Ref (Either Address Coords)
location = ref "location"

pressure :: FRP Topic Pressure
pressure = topic "pressure"

temperature :: FRP Topic Temperature
temperature = topic "temperature"

wind :: FRP Topic Wind
wind = topic "wind"

person :: FRP Ref (FirstName, (LastName, Either Address Coords))
person = firstName |.| lastName |.| location

writePerson :: FRP WriteEntity (FirstName, (LastName, Either Address Coords))
writePerson = writeRef' person

writeReversedFirstName :: FRP WriteEntity a
writeReversedFirstName = undefined >$< writePerson -- TODO

readPerson :: FRP ReadEntity (FirstName, (LastName, Either Address Coords))
readPerson = readRef' $ firstName |.| lastName |.| location

readFirstName :: FRP ReadEntity FirstName
readFirstName = readRef' firstName

readLastName :: FRP ReadEntity LastName
readLastName = readRef' lastName

readFirstAndLastName :: FRP ReadEntity (FirstName, LastName)
readFirstAndLastName = (,) <$> readFirstName <*> readLastName

readAddress :: FRP ReadEntity Address
readAddress = fst $ expand $ readRef' location

readCoords :: FRP ReadEntity Coords
readCoords = snd $ expand $ readRef' location

readFive :: FRP ReadEntity Int
readFive = constant 5

subscribeWheatherInfo :: FRP SubscribeStream (Either Pressure (Either Temperature Wind))
subscribeWheatherInfo = subscribeTopic' $ pressure |.| temperature |.| wind

subscribePressure :: FRP SubscribeStream Pressure
subscribePressure = subscribeTopic' pressure

subscribeTemperature :: FRP SubscribeStream Temperature
subscribeTemperature = subscribeTopic' temperature

subscribeAdjustedPressure :: FRP SubscribeStream Pressure
subscribeAdjustedPressure = (+ 10) <$> subscribePressure

subscribePressureAndTemperature :: FRP SubscribeStream (Either Pressure Temperature)
subscribePressureAndTemperature = subscribePressure |.| subscribeTemperature

subscribeEmpty :: FRP SubscribeStream Int
subscribeEmpty = empty @Int

writeWhetherInfo :: FRP WriteStream (Either Pressure (Either Temperature Wind))
writeWhetherInfo = writeTopic' $ pressure |.| temperature |.| wind

writePressure :: FRP WriteStream Pressure
writePressure = writeTopic' pressure

writeAdjustedPressure :: FRP WriteStream Pressure
writeAdjustedPressure = (+ 10) >$< writePressure

writeTemeprature :: FRP WriteStream Temperature
writeTemeprature = writeTopic' temperature

writeWind :: FRP WriteStream Wind
writeWind = writeTopic' wind

temperatureWithPerson :: FRP Topic ((FirstName, (LastName, Either Address Coords)), Temperature)
temperatureWithPerson = person |>| temperature

temperatureWithAddress :: FRP SubscribeStream (Address, Temperature)
temperatureWithAddress = readAddress |>| subscribeTemperature

writePersonAndTemperature :: FRP WriteStream ((FirstName, (LastName, Either Address Coords)), Temperature)
writePersonAndTemperature = writePerson |>| writeTemeprature

foo :: FRP WriteStream ((FirstName, (LastName, Either Address Coords)), Void)
foo = writePerson |>| none

main :: IO ()
main = runFRP $ do
  writeEntity writePerson (Just ("Paul", ("Smith", Right "50N20E")))
  readEntity readFirstName >>= liftIO . print
  readEntity readLastName >>= liftIO . print
  readEntity readAddress >>= liftIO . print
  readEntity readCoords >>= liftIO . print
  subscribeStream subscribeWheatherInfo print
  subscribeStream subscribePressure print
  subscribeStream subscribeTemperature print
  subscribeStream subscribeAdjustedPressure print
  subscribeStream subscribePressureAndTemperature print
  liftIO getLine; writeStream writePressure 1002
  liftIO getLine; writeStream writeTemeprature 23.8
  liftIO getLine; writeStream writeWind 23.8
  return ()
