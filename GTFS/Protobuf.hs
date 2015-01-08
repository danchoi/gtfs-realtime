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
    gtfsRealTimeVersion  :: Required 1 (Value Text)
  , incrementality       :: Optional 2 (Enumeration Incrementality)
  , feedHeaderTimestamp  :: Optional 3 (Value Int64)
  } deriving (Generic, Show)

instance Decode FeedHeader

data Incrementality = FullDataset | Differential 
  deriving (Show, Ord, Eq, Enum)

data FeedEntity = FeedEntity {
    feedId         :: Required 1 (Value Text)
  , feedIsDeleted  :: Optional 2 (Value Bool)
  , tripUpdate     :: Optional 3 (Message TripUpdate)
  , vehicle        :: Optional 4 (Message VehiclePosition)
  , alert          :: Optional 5 (Message Alert)
  } deriving (Generic, Show)

instance Decode FeedEntity

------------------------------------------------------------------------

data TripDescriptor = TripDescriptor {
    trip_id                :: Optional 1 (Value Text)
  , route_id               :: Optional 5 (Value Text)
  , start_time             :: Optional 2 (Value Text)
  , start_date             :: Optional 3 (Value Text)
  , schedule_relationship  :: Optional 4 (Enumeration TDScheduleRelationship)
  -- extensions
  } deriving (Generic, Show)

instance Decode TripDescriptor

data TripUpdate = TripUpdate {
    tuTrip           :: Required 1 (Message TripDescriptor)
  , tuVehicle        :: Optional 3 (Message VehicleDescriptor)
  , tuStopTimeUpdate :: Repeated 2 (Message StopTimeUpdate)
  , tuTimeStamp      :: Optional 4 (Value Int64)
  } deriving (Generic, Show)

instance Decode TripUpdate 

data VehicleDescriptor = VehicleDescriptor {
    vehicleId    :: Optional 1 (Value Text)
  , vehicleLabel :: Optional 2 (Value Text)
  , licensePlate :: Optional 3 (Value Text)
  -- extensions
  } deriving (Generic, Show)

instance Decode VehicleDescriptor 

data StopTimeUpdate = StopTimeUpdate {
    stop_sequence          :: Optional 1 (Value Int32)
  , stop_id                :: Optional 4 (Value Text)
  , arrival                :: Optional 2 (Message StopTimeEvent)
  , departure              :: Optional 3 (Message StopTimeEvent)
  , stScheduleRelationship :: Optional 5 (Enumeration STScheduleRelationship)  -- note default is not implemented yet in library
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

------------------------------------------------------------------------

data VehiclePosition = VehiclePosition {
    vpTrip                   :: Optional 1 (Message TripDescriptor)
  , vpVehicle                :: Optional 3 (Message VehicleDescriptor)
  , vpPosition               :: Optional 2 (Message Position)
  , vpCurrentStopSequence    :: Optional 3 (Value Int32)
  , vpStopId                 :: Optional 7 (Value String)
  , vpVehicleStopStatus      :: Optional 4 (Enumeration VehicleStopStatus)
  , vpTimeStamp              :: Optional 5 (Value Int64)
  , vpCongestionLevel        :: Optional 6 (Enumeration  CongestionLevel)
  } deriving (Generic, Show)

instance Decode VehiclePosition
                            
data Position = Position {
    latitude                 :: Required 1 (Value Float)
  , longitude                :: Required 2 (Value Float)
  , bearing                  :: Optional 3 (Value Float)
  , odometer                 :: Optional 4 (Value Double)
  , speed                    :: Optional 5 (Value Float)
  } deriving (Generic, Show)
                            
instance Decode Position

data VehicleStopStatus = IncomingAt | StoppedAt | InTransitTo 
  deriving (Show, Ord, Eq, Enum)

data CongestionLevel = UnknownCongestionLevel 
                     | RunningSmoothly
                     | StopAndGo
                     | Congestion
                     | SevereCongestion
                     deriving (Show, Ord, Eq, Enum)

------------------------------------------------------------------------

data Alert = Alert {
    alertTimeRange           :: Repeated 1 (Message TimeRange)
  , alertEntitySelector      :: Repeated 5 (Message EntitySelector)
  , alertCause               :: Optional 6 (Enumeration Cause)
  , alertEffect              :: Optional 7 (Enumeration Effect)
  , alertUrl                 :: Optional 8 (Message TranslatedString)
  , alertDescriptionText     :: Optional 11 (Message TranslatedString)
  } deriving (Generic, Show)

instance Decode Alert
                            
data TimeRange = TimeRange {
    timeRangeStart :: Optional 1 (Value Int64)
  , timeRangeEnd   :: Optional 2 (Value Int64)
  } deriving (Generic, Show)

instance Decode TimeRange

data EntitySelector = EntitySelector {
    esAgencyId :: Optional 1 (Value String)
  , esRouteId :: Optional 2 (Value String)
  , esRouteType :: Optional 3 (Value Int32)
  , esTrip :: Optional 4 (Message TripDescriptor)
  , esStopId :: Optional 5 (Value String)
  } deriving (Generic, Show)

instance Decode EntitySelector

data Cause = DUMMYCAUSE -- to force the real enumeration to start at 1
           | UnknownCause
           | OtherCause
           | TechnicalProblem
           | Strike
           | Demonstration
           | Accident
           | Holiday
           | Weather
           | Maintenance
           | Construction
           | PoliceActivity
           | MedicalEmergency
           deriving (Show, Ord, Eq, Enum)

data Effect = DUMMYEFFECT
            | NoService
            | ReducedService
            | SignificantDelays
            | Detour
            | AdditionalService
            | ModifiedService
            | OtherEffect
            | UnknownEffect
            | StopMoved
            deriving (Show, Ord, Eq, Enum)

data TranslatedString = TranslatedString {
    translation :: Repeated 1 (Message Translation)
    } deriving (Generic, Show)

instance Decode TranslatedString

data Translation = Translation {
    translationText     :: Required 1 (Value String)
  , translationLanguage :: Optional 2 (Value String)
  } deriving (Generic, Show)

instance Decode Translation

