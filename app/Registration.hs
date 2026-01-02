module Registration where
import Network.Wai (Request, getRequestBodyChunk)
import Database.PostgreSQL.Simple (Connection, execute)
import DataSchema (NixMachine, NixMachineEnrollment (enrollmentToken), toNixMachine)
import qualified Data.ByteString.Char8 as BS
import Control.Monad (void, when)
import DatabaseInterface (registerMachineQuery)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import System.IO (openFile, IOMode (WriteMode), hClose)


enrollmentTokenFilePath :: String
enrollmentTokenFilePath = "./enrollment_tokens"

createEnrollmentTokenFile :: Bool -> IO ()
createEnrollmentTokenFile isPresent = if isPresent then return ()
                                       else openFile enrollmentTokenFilePath WriteMode >>= hClose

registerMachine :: Request -> Connection -> IO Bool
registerMachine request conn = do
  maybenixMachine       <- processNixMachineEnrollRequest request BS.empty
  case maybenixMachine of
    Just nixMachineEnrollment -> enrollmentTokenSearch (enrollmentToken nixMachineEnrollment) >>= \isValid ->
        executeRegistration isValid conn $ toNixMachine nixMachineEnrollment
    Nothing  -> return False

executeRegistration :: Bool -> Connection -> NixMachine -> IO Bool
executeRegistration isValidToken conn nixMachine = when isValidToken (void $ execute conn registerMachineQuery nixMachine) >> return True

deregisterMachine :: Connection -> String -> ()
deregisterMachine = undefined

-- search file for enrollment token, in time perhaps this will live in the DB, local file for now
enrollmentTokenSearch :: String -> IO Bool
enrollmentTokenSearch submission = elem submission . lines <$> readFile enrollmentTokenFilePath


-- maybe i can generalize this 
processNixMachineEnrollRequest :: Request -> BS.ByteString -> IO (Maybe NixMachineEnrollment)
processNixMachineEnrollRequest request bstr = do
  chunk <- getRequestBodyChunk request
  let newBstr = BS.append bstr chunk
  let decodeAttempt = decode $ BSL.fromStrict newBstr

  if BS.length newBstr > 1024
    then return Nothing
  else if isJust decodeAttempt
    then return decodeAttempt
    else processNixMachineEnrollRequest request newBstr





