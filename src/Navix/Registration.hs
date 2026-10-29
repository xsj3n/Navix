{-# LANGUAGE OverloadedStrings #-}
module Navix.Registration where 
import Network.Wai (Request (requestBodyLength), getRequestBodyChunk, RequestBodyLength (ChunkedBody, KnownLength), strictRequestBody)
import Database.PostgreSQL.Simple (Connection, execute)
import Navix.DataSchema (NixMachine, NixMachineEnrollment (enrollmentToken, machineDetails), expiresAt, TokenValidityInfo (revokedAt, usedAt))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Control.Monad (void, when)
import Navix.DatabaseInterface (insertToken, getTokenInfo, DatabaseConnection (dbConn), insertMachine)
import Data.Aeson (decode)
import Data.ByteArray.Encoding (convertToBase, Base (Base16))
import Data.Maybe
import System.IO (openFile, IOMode (WriteMode), hClose)
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteArray as BA
import Data.Time (getCurrentTime)
import qualified Data.List.NonEmpty as NE

registerMachine :: Request -> DatabaseConnection -> IO Bool
registerMachine request conn = do
  maybeNixMachine       <- processNixMachineEnrollRequest request
  if isNothing maybeNixMachine then return False
  else do
    let nixMachine = fromJust maybeNixMachine
        tokenHash  = hash $ BSC.pack $ enrollmentToken nixMachine :: Digest SHA256
    handleTokenValidation (dbConn conn) $ BA.convert tokenHash
    insertMachine conn nixMachine

   

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
    


-- token stuff 
handleTokenValidation :: Connection -> BS.ByteString -> IO Bool
handleTokenValidation conn tokenHash = do
  tokenInfoM   <- getTokenInfo conn tokenHash >>= return . NE.nonEmpty 
  if isNothing tokenInfoM then return False
  else do
    let tokenInfo = NE.head $ fromJust tokenInfoM
    currentTime <- getCurrentTime
    if (expiresAt tokenInfo < currentTime) ||
       (isJust $ revokedAt tokenInfo)      ||
       (isJust $ usedAt tokenInfo)
    then return False
    else return True 


generateToken :: Connection -> IO (BS.ByteString, String)
generateToken conn = do
  randBytes <- getRandomBytes 48 :: IO BS.ByteString
  let token          = "enr_" <> (convertToBase Base16 randBytes :: BS.ByteString)
      tokenHash      = BA.convert (hash token :: Digest SHA256)
  insertToken conn tokenHash
  return (tokenHash, BSC.unpack token)
