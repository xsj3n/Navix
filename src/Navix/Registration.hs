module Navix.Registration where
import Network.Wai (Request, getRequestBodyChunk)
import Database.PostgreSQL.Simple (Connection, execute)
import Navix.DataSchema (NixMachine, NixMachineEnrollment (enrollmentToken, machineDetails))
import qualified Data.ByteString.Char8 as BS
import Control.Monad (void, when)
import Navix.DatabaseInterface (getRegistrationQuery)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import System.IO (openFile, IOMode (WriteMode), hClose)



registerMachine :: Request -> Connection -> IO Bool
registerMachine request conn = do
  maybenixMachine       <- processNixMachineEnrollRequest request BS.empty
  case maybenixMachine of
    Just nixMachineEnrollment -> enrollmentTokenSearch (enrollmentToken nixMachineEnrollment) >>= \isValid ->
        executeRegistration isValid conn $ machineDetails nixMachineEnrollment
    Nothing  -> return False

executeRegistration :: Bool -> Connection -> NixMachine -> IO Bool
executeRegistration isValidToken conn nixMachine = do
  query <- getRegistrationQuery
  when isValidToken (void $ execute conn query nixMachine) >> return True

deregisterMachine :: Connection -> String -> ()
deregisterMachine = undefined


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





