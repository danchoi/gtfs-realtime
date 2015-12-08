{-# LANGUAGE OverloadedStrings #-}
module GTFS.JSON where
import GTFS.Protobuf
import Data.Aeson hiding (Value)
import qualified Data.Text as T
import Data.ProtocolBuffers

instance ToJSON TripDescriptor where
  toJSON v = object [
      "trip_id"               .= (getField . trip_id $ v)
    , "route_id"              .= (getField . route_id $ v)
    , "start_time"            .= (getField . start_time $ v)
    , "start_date"            .= (getField . start_date $ v)
    , "schedule_relationship" .= (getField . schedule_relationship $ v)
    ]

instance ToJSON TripUpdate where
  toJSON v = object [
      "vehicle"           .= (getField . tuVehicle $ v)
    , "trip"              .= (getField . tuTrip $ v)
    , "stop_time_updates" .= (getField . tuStopTimeUpdate $ v)
    ]

instance ToJSON VehicleDescriptor where
  toJSON v = object [
      "id"    .= (getField . vehicleId $ v)
    , "label" .= (getField . vehicleLabel $ v)
    ]

instance ToJSON StopTimeUpdate where
  toJSON v = object [
      "stop_sequence"         .= (getField . stop_sequence $ v)
    , "stop_id"               .= (getField . stop_id $ v)
    , "arrival"               .= (getField . arrival $ v)
    , "departure"             .= (getField . departure $ v)
    , "schedule_relationship" .= (getField . stScheduleRelationship $ v)
    ]

instance ToJSON TDScheduleRelationship where toJSON = String . T.pack . show 

instance ToJSON STScheduleRelationship where toJSON = String . T.pack . show 
    
instance ToJSON StopTimeEvent where
  toJSON v = object [
      "delay"       .= (getField . delay $ v)
    , "time"        .= (getField . time $ v)
    , "uncertainty" .= (getField . uncertainty $ v)
    ]

------------------------------------------------------------------------

instance ToJSON VehiclePosition where
  toJSON v = object [
      "trip" .= (getField . vpTrip $ v)
    , "vehicle" .= (getField . vpVehicle $ v)
    , "position" .= (getField . vpPosition $ v)
    , "current_stop_sequence" .= (getField . vpCurrentStopSequence $ v)
    , "stop_status" .= (getField . vpVehicleStopStatus $ v)
    , "timestamp" .= (getField . vpTimeStamp $ v)
    , "congestion_level" .= (getField . vpCongestionLevel $ v)
    ] 

instance ToJSON Position where
  toJSON v = object [
      "lat" .= (getField . latitude $ v)
    , "lng" .= (getField . longitude $ v)
    , "bearing" .= (getField . bearing $ v)
    , "odometer" .= (getField . odometer $ v)
    , "speed" .= (getField . speed $ v)
    ] 

instance ToJSON VehicleStopStatus where
  toJSON = String . T.pack . show

instance ToJSON CongestionLevel where
  toJSON = String . T.pack . show

------------------------------------------------------------------------

instance ToJSON Alert where
  toJSON v = object [
      "time_range" .= (getField . alertTimeRange $ v)
    , "entities" .= (getField . alertEntitySelector $ v)
    , "cause" .= (getField . alertCause $ v)
    , "effect" .= (getField . alertEffect $ v)
    , "url" .= (getField . alertUrl $ v)
    , "description"  .= (getField . alertDescriptionText $ v)
    ]

instance ToJSON TimeRange where
  toJSON v = object [
      "start" .= (getField . timeRangeStart $ v)
    , "end" .= (getField . timeRangeEnd $ v)
    ]

instance ToJSON EntitySelector where
  toJSON v = object [
      "agency_id" .= (getField . esAgencyId $ v)
    , "route_id" .= (getField . esRouteId  $ v)
    , "route_type" .= (getField . esRouteType  $ v)
    , "trip" .= (getField . esTrip  $ v)
    , "stop_id" .= (getField . esStopId  $ v)
    ]
    
instance ToJSON Cause where
  toJSON = String . T.pack . show

instance ToJSON Effect where
  toJSON = String . T.pack . show
  
instance ToJSON TranslatedString where
  toJSON v = toJSON (getField . translation $ v)

instance ToJSON Translation where
  toJSON v = object [ 
        "text" .= (getField . translationText $ v)
      , "language" .= (getField . translationLanguage $ v)
      ]


