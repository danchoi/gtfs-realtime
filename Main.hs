module Main where
import qualified GTFS.Protobuf as P
import GTFS.JSON
import Data.ProtocolBuffers  hiding (encode)
import Data.Serialize hiding (encode)
import Data.Hex
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Control.Monad
import Data.Aeson (encode)
import System.Environment (getArgs)

main = do 
  raw <- BL.getContents
  let res = runGetLazy decodeMessage raw :: Either String P.FeedMessage
  [feedtype] <- getArgs
  case res of
    Left err -> error $ "error: " ++ err
    Right feed -> do
      let xs = getField $ P.feedEntity feed               
      case feedtype of
        "t" -> mapM_ (BL8.putStrLn . encode . getField . P.tripUpdate) xs 
        "a" -> mapM_ (BL8.putStrLn . encode . getField . P.alert) xs 
        "v" -> mapM_ (BL8.putStrLn . encode . getField . P.vehicle) xs 
  
