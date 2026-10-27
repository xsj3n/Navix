module Navix.Registration where 
import Network.Wai (Request (requestBodyLength), getRequestBodyChunk, RequestBodyLength (ChunkedBody, KnownLength), strictRequestBody)
import Database.PostgreSQL.Simple (Connection, execute)
import Navix.DataSchema (NixMachine, NixMachineEnrollment (enrollmentToken, machineDetails))
import Data.ByteString
import qualified Data.ByteString.Char8 as BSC
import Control.Monad (void, when)
import Navix.DatabaseInterface (getRegistrationQuery, insertToken, getTokenInfo)
import Data.Aeson (decode)
import Data.ByteArray.Encoding (convertToBase, Base (Base16))
import Data.Maybe
import System.IO (openFile, IOMode (WriteMode), hClose)
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteArray as BA

registerMachine :: Request -> Connection -> IO Bool
registerMachine request conn = do
  maybeNixMachine       <- processNixMachineEnrollRequest request
  if isNothing maybeNixMachine then return False
  else do
    let nixMachine = fromJust maybeNixMachine
        tokenHash  = hash $ BSC.pack $ enrollmentToken nixMachine :: Digest SHA256
    isValidated <- handleTokenValidation conn $ BA.convert tokenHash 
    undefined

handleTokenValidation :: Connection -> ByteString -> IO Bool
handleTokenValidation conn tokenHash = do
  tokenInfo <- getTokenInfo conn tokenHash
  undefined

   
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

generateToken :: Connection -> IO (ByteString, String)
generateToken conn = do
  randBytes <- getRandomBytes 48
  let token          = convertToBase Base16 randBytes
      tokenHash      = BA.convert (hash token :: Digest SHA256)

  insertToken conn tokenHash   
  return (tokenHash, token)
