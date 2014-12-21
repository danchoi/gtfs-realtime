module Main where
import qualified GTFS.Protobuf as P
import GTFS.Types
import Data.ProtocolBuffers
import Data.Serialize
import Data.Hex
import qualified Data.ByteString.Lazy as BL
import Control.Monad

main = do 
    raw <- BL.getContents
    let res = runGetLazy decodeMessage raw :: Either String P.FeedMessage
    case res of
      Left err -> error $ "error: " ++ err
      Right feed -> do
        let xs = getField $ P.feedEntity feed               
        mapM_ (print . f . getField . P.tripUpdate) xs 
 where f t = case t of 
              Just t' -> decodeTripUpdate t'
              Nothing -> error "Can't decode trip update"


    
