module Navix.PollResponse where
import Network.Wai (Request, getRequestBodyChunk)
import Database.PostgreSQL.Simple (Connection)
import qualified Data.ByteString as BS
import Navix.DataSchema (Sha256)


readBytes256 :: BS.ByteString -> Request -> IO (Either Sha256 String)
readBytes256 bstr request = do
  chunk <- getRequestBodyChunk request
  let newBstr = BS.append bstr chunk
  let len     =  BS.length newBstr
  
  if len > 512
    then return $ Right "Input contains too many bytes"
  else if len >= 256
    then return $ Left newBstr else
   if len < 256 && not (BS.null chunk)
    then readBytes256 newBstr request
    else return $ Right "Input does not contain enough bytes"

    
-- pollResponse :: Request -> Connection -> IO Bool
-- pollResponse request conn = do
--   sha <- readBytes256 BS.empty request
    



