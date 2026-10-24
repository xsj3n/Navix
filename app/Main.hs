{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Navix.DatabaseInterface as DB
import Navix.DataSchema
import Navix.Registration
import Navix.TLS
import qualified Navix.PKI as PKI

import qualified Network.TLS as TLS
import Network.Wai
import Network.Wai.Handler.WarpTLS
import Network.Wai.Handler.WarpTLS.Internal
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Prelude
import Data.UUID (UUID)
import Data.Aeson
import System.Exit
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Database.PostgreSQL.Simple as SQL
import Control.Monad (void, when, unless)
import Data.Int (Int64)
import System.IO (IOMode(WriteMode, ReadMode), openFile, hClose)
import System.Directory (doesFileExist, listDirectory)
import System.Directory.Extra (listFiles)
import Data.Maybe
import Network.TLS.Extra (ciphersuite_strong)
import qualified System.Process as OS
import qualified System.Environment as ENV
import Control.Monad.Extra (unlessM, whenM)
import Data.List.Extra (splitOn)
import Data.List
import System.Directory (copyFile)
import Database.PostgreSQL.Simple (Connection)


isRegisterRequest :: Method -> String -> Bool
isRegisterRequest method action = method == methodPost && action == "register"

isDeregisterRequest :: Method -> String -> Bool
isDeregisterRequest method action = method == methodPost && action == "deregister"

isPoll :: Method -> String -> Bool
isPoll method action = method == methodGet && action == "poll"

main :: IO ()
main = do

   -- TODO: implement allowances for pre-configured   
  whenM (null <$> listFiles PKI.certStoreDir) PKI.initPKI  
  failuresList <- PKI.verifyPKI "Navix.local" 
  unless (null failuresList) (PKI.printAllReasons failuresList >> exitFailure)

  -- enrollment tokens kept in file for now
  doesFileExist enrollmentTokenFilePath >>= createEnrollmentTokenFile

  
  databaseConnection <- DB.getDatabaseConnection
  -- maybe this should be the same as whats kept in the cert-store, maybe not
  -- let tlsOpts = mutualTLSSettings PKI.tlsCertPath PKI.tlsKeyPath
  let tlsOpts = tlsSettings PKI.tlsCertPath PKI.tlsKeyPath
  let port = 3000

  putStrLn $ "[*] Listening on https://localhost:" ++ show port
  runTLS tlsOpts (setPort port defaultSettings) $ app databaseConnection

-- introduce pooled connections at some point 
app :: Connection -> Application
app connection request respond
  | isRegisterRequest method action = registerMachine request connection >>= (respond . simpleResponse)
  | isDeregisterRequest method action = undefined
  | isPoll method action = undefined
  | otherwise = respond $ responseLBS status400 [] "Malformed request"
  where
    method = requestMethod request
    action = T.unpack $ last $ pathInfo request




-- place holder simple response 
simpleResponse :: Bool -> Response
simpleResponse isSuccess = if isSuccess
                             then responseLBS status200 [] "Success"
                             else responseLBS status400 [] "Invalid"



