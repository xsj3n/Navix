module Navix.Registration where
import Network.Wai (Request (requestBodyLength), getRequestBodyChunk, RequestBodyLength (ChunkedBody, KnownLength), strictRequestBody)
import Database.PostgreSQL.Simple (Connection, execute)
import Navix.DataSchema (NixMachine, NixMachineEnrollment (enrollmentToken, machineDetails))
import qualified Data.ByteString.Char8 as BS
import Control.Monad (void, when)
import Navix.DatabaseInterface (getRegistrationQuery)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteArray.Encoding (convertToBase, Base (Base16))
import Data.Maybe
import System.IO (openFile, IOMode (WriteMode), hClose)
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.Random


registerMachine :: Request -> Connection -> IO Bool
registerMachine request conn = do
  maybenixMachine       <- processNixMachineEnrollRequest request
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
processNixMachineEnrollRequest :: Request -> IO (Maybe NixMachineEnrollment)
processNixMachineEnrollRequest request  = do 
  let len = case requestBodyLength request of 
              ChunkedBody   -> 0
              KnownLength l -> l

  if len > 4028 || len == 0 then return Nothing
  else do
    body <- strictRequestBody request
    case (decode body) of
      Nothing -> return Nothing
      Just n  -> return $ Just n 
    



--- token stuff

generateToken :: IO (String)
generateToken = do
  randBytes <- getRandomBytes 48
  let token      = convertToBase Base16 randBytes
      tokenHash  = hash token :: Digest SHA256

  

  return token 
