module GTFS.Types
where 
import Data.Text (Text)
import qualified GTFS.Protobuf as P
import Data.ProtocolBuffers

data TripUpdate = TripUpdate {
    trip :: Trip
  , vehicle :: (Maybe Vehicle)
  , stopTimeUpdate :: [StopTimeUpdate]
  } deriving (Show)

-- decodeTripUpdate :: P.TripUpdate -> TripUpdate
decodeTripUpdate x = 
    let tripDescriptor' = getField . P.trip $ x
        vehicle'  = getField . P.vehicle $ x
        stopTimeUpdates' = getField . P.stop_time_update $ x
    in (tripDescriptor', vehicle', stopTimeUpdates')

decodeTrip :: P.TripDescriptor -> Trip
decodeTrip x = undefined
    

data Trip = Trip Int Int Text Text Text deriving (Show)

data Vehicle = Vehicle Int Text Text deriving (Show)

data StopTimeUpdate = StopTimeUpdate 
  (Maybe Int)
  (Maybe Text)
  (Maybe StopTimeEvent)
  (Maybe StopTimeEvent) deriving (Show)

data StopTimeEvent = StopTimeEvent
  (Maybe Int ) (Maybe Int) (Maybe Int)
  deriving Show
