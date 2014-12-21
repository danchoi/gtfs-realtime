module Main where
import GTFS.Test
import Data.ProtocolBuffers
import Data.Serialize
import Data.Hex
import qualified Data.ByteString.Lazy as BL
import Control.Monad

main = do 
    raw <- BL.getContents
    let res = runGetLazy decodeMessage raw :: Either String FeedMessage
    case res of
      Left err -> error $ "error: " ++ err
      Right feed -> do
        let xs = getField $ feedEntity feed               
        mapM_ (print . getField . tripUpdate) xs


    
