{-# LANGUAGE OverloadedStrings #-}

module Main where
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
import Navix.DatabaseInterface
import Navix.DataSchema
import Database.PostgreSQL.Simple
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
import Navix.Registration
import qualified Navix.PKI as PKI


-- mutualTLSSettings :: TLSSettings
-- mTLSSettings = defaultTlsSetings
--   { tlsWantClientCert = True
--   , tlsServerHooks = TLS.defaultServerHooks
--   }

-- clientCertificateHandler :: 


-- mutualTLSServerHooks :: ServerHooks
-- mTLSServerHooks = TLS.defaultServerHooks
--  { onClientCertificate = \chain -> if 
--   }

main :: IO ()
main = do
  
  whenM (null <$> listFiles "./cert-store/") PKI.initPKI  
  pkiTransportStatus <- PKI.chkTransportCertAndKey
  
  _ <- case pkiTransportStatus of
    PKI.TransOkay         -> return Nothing
    PKI.TransCheckError   -> error "[!] Error: this should never happen wtf" >> return Nothing
    PKI.KeyAndCertMissing -> undefined
    PKI.CertMissing       -> undefined
    PKI.KeyMissing        -> undefined

  doesFileExist enrollmentTokenFilePath >>= createEnrollmentTokenFile

  -- Create db and default table if db is absent
  superUserConnection <- getSuperUserConnection
  putStrLn "[*] Connected to database as superuser for database check"
  [Only (result :: Bool)] <- query_ superUserConnection checkForDatabaseQuery
  unless result $ void (execute_ superUserConnection createDatabaseQuery :: IO Int64)
  close superUserConnection

  databaseConnection <- getConnection
  unless result $ void (execute_ databaseConnection createMachinesTableQuery :: IO Int64)

  -- maybe this should be the same as whats kept in the cert-store, maybe not
  let tlsOpts = tlsSettings "cert.pem" "key.pem"
  let port = 3000

  putStrLn $ "[*] Listening on https://localhost:" ++ show port
  runTLS tlsOpts (setPort port defaultSettings) $ app databaseConnection

-- introduce pooled connections at some point 
app :: Connection -> Application
app connection request respond
  | reqType == methodPost && action == "register" = registerMachine request connection >>= (respond . simpleResponse)
  | reqType == methodPost && action == "deregister" = undefined
  | reqType == methodGet && action == "poll" = undefined
  | otherwise = respond $ responseLBS status400 [] "Malformed request"
  where
    reqType = requestMethod request
    action = T.unpack $ last $ pathInfo request





-- place holder simple response 
simpleResponse :: Bool -> Response
simpleResponse isSuccess = if isSuccess
                             then responseLBS status200 [] "Success"
                             else responseLBS status400 [] "Invalid"



