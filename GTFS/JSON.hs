{-# LANGUAGE OverloadedStrings #-}
module GTFS.JSON where
import GTFS.Protobuf
import Data.Aeson hiding (Value)
import qualified Data.Text as T
import Data.ProtocolBuffers

instance ToJSON TripDescriptor where
  toJSON v = object [
      "trip_id" .= (getField . trip_id $ v)
    , "route_id" .= (getField . route_id $ v)
    , "start_time" .= (getField . start_time $ v)
    , "start_date" .= (getField . start_date $ v)
    , "schedule_relationship" .= (getField . schedule_relationship $ v)
    ]

instance ToJSON TripUpdate where
  toJSON v = object [
      "vehicle" .= (getField . vehicle $ v)
    , "trip" .= (getField . trip $ v)
    , "stop_time_updates" .= (getField . stop_time_update $ v)
    ]

instance ToJSON VehicleDescriptor where
  toJSON v = object [
      "id" .= (getField . vehicleId $ v)
    , "label" .= (getField . vehicleLabel $ v)
    ]

instance ToJSON StopTimeUpdate where
  toJSON v = object [
      "stop_sequence" .= (getField . stop_sequence $ v) 
    , "stop_id" .= (getField . stop_id $ v)
    , "arrival" .= (getField . arrival $ v)
    , "departure" .= (getField . departure $ v)
    , "schedule_relationship" .= (getField . stScheduleRelationship $ v)
    ]

instance ToJSON TDScheduleRelationship where toJSON = String . T.pack . show 

instance ToJSON STScheduleRelationship where toJSON = String . T.pack . show 
    
instance ToJSON StopTimeEvent where
  toJSON v = object [
      "delay" .= (getField . delay $ v)
    , "time" .= (getField . time $ v)
    , "uncertainty" .= (getField . uncertainty $ v)
    ]



