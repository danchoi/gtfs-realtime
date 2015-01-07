{-# LANGUAGE OverloadedStrings, DeriveGeneric, DataKinds #-}
module GTFS.Protobuf where
import Data.Int
import Data.ProtocolBuffers
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.TypeLits

data FeedMessage = FeedMessage {
    feedHeader :: Required 1 (Message FeedHeader)
  , feedEntity :: Repeated 2 (Message FeedEntity)
  } deriving (Generic, Show)

instance Decode FeedMessage

data FeedHeader = FeedHeader {
    gtfsRealTimeVersion :: Required 1 (Value Text)
  , incrementality :: Optional 2 (Enumeration Incrementality)
  , feedHeaderTimestamp :: Optional 3 (Value Int64)
  } deriving (Generic, Show)

instance Decode FeedHeader

data Incrementality = FullDataset | Differential deriving (Show, Ord, Eq, Enum)

data FeedEntity = FeedEntity {
    feedId :: Required 1 (Value Text)
  , feedIsDeleted :: Optional 2 (Value Bool)
  , tripUpdate :: Optional 3 (Message TripUpdate)
  } deriving (Generic, Show)

instance Decode FeedEntity

data TripDescriptor = TripDescriptor {
    trip_id :: Optional 1 (Value Text)
  , route_id :: Optional 5 (Value Text)
  , start_time :: Optional 2 (Value Text)
  , start_date :: Optional 3 (Value Text)
  , schedule_relationship :: Optional 4 (Enumeration TDScheduleRelationship)
  -- extensions
  } deriving (Generic, Show)

instance Decode TripDescriptor

data TripUpdate = TripUpdate {
    trip :: Required 1 (Message TripDescriptor)
  , vehicle :: Optional 3 (Message VehicleDescriptor)
  , stop_time_update :: Repeated 2 (Message StopTimeUpdate)
  , timestamp :: Optional 4 (Value Int64)
  } deriving (Generic, Show)

instance Decode TripUpdate 

data VehicleDescriptor = VehicleDescriptor {
    vehicleId :: Optional 1 (Value Text)
  , vehicleLabel :: Optional 2 (Value Text)
  , licensePlate :: Optional 3 (Value Text)
  -- extensions
  } deriving (Generic, Show)

instance Decode VehicleDescriptor 

data StopTimeUpdate = StopTimeUpdate {
    stop_sequence :: Optional 1 (Value Int32)
  , stop_id :: Optional 4 (Value Text)
  , arrival :: Optional 2 (Message StopTimeEvent)
  , departure :: Optional 3 (Message StopTimeEvent)
  -- note default is not implemented yet in library
  , stScheduleRelationship :: Optional 5 (Enumeration STScheduleRelationship)
  } deriving (Generic, Show)

instance Decode StopTimeUpdate 

data TDScheduleRelationship = Scheduled | Added | Unscheduled | Canceled 
  deriving (Show, Eq, Ord, Enum)

data STScheduleRelationship = Schedules | Skipped | NoData 
  deriving (Show, Eq, Ord, Enum)

data StopTimeEvent = StopTimeEvent {
    delay :: Optional 1 (Value Int32)
  , time :: Optional 2 (Value Int64)
  , uncertainty :: Optional 3 (Value Int32)
  -- extensions?
  } deriving (Generic, Show)

instance Decode StopTimeEvent

