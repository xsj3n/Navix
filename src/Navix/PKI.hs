{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Navix.PKI where

import System.Process (callCommand)
import Data.String.Interpolate
import System.Directory (doesFileExist)
import Data.X509
import Data.X509.Validation
import Data.X509.CertificateStore
import System.Directory.Extra (doesDirectoryExist, createDirectory)
import Control.Monad (unless)
import Control.Monad.Extra
import qualified Data.ByteString as BS
import Data.X509.File (readSignedObject)
import Data.Foldable (for_)

certStoreDir :: FilePath
certStoreDir = "./cert-store/"

tlsPKIDir :: FilePath
tlsPKIDir = certStoreDir ++ "transport/"

rootCertPath :: FilePath
rootCertPath = certStoreDir ++ "root-crt.pem"

rootKeyPath :: FilePath
rootKeyPath = certStoreDir ++ "root-key.pem"

tlsCertPath :: FilePath
tlsCertPath = tlsPKIDir ++ "tls-crt.pem"

tlsKeyPath :: FilePath
tlsKeyPath = tlsPKIDir ++ "tls-key.pem"

tlsCSRPath :: FilePath
tlsCSRPath = tlsPKIDir ++ "tls-csr.pem"

defaultRootSubject :: String
defaultRootSubject = "\"/C=/ST=/L=/O=/CN=Navix\""

defaultTLSSubject :: String
defaultTLSSubject = "\"/C=/ST=/L=/O=/CN=Navix.local\""


-- make configurable later
defaultSAN :: String
defaultSAN = "\"subjectAltName=DNS:Navix.local,DNS:www.Navix.local\""

type CurveType = String 
curveConfig :: String
curveConfig = "prime256v1"


createECKey :: CurveType -> FilePath -> IO ()
createECKey curve path = callCommand $
  [i|openssl ecparam -name #{curve} -genkey -noout -out #{path}|]


createSelfSignedCert :: FilePath -> FilePath -> IO ()
createSelfSignedCert inputKeyPath outputCertPath= callCommand $
  [iii|
  openssl req -x509 -new -noenc -sha256 -days 3650
  -key #{inputKeyPath}
  -out #{outputCertPath}
  -subj #{defaultRootSubject}
  |]

createTLSCSR :: FilePath -> FilePath -> IO ()
createTLSCSR inputKeyPath outputCSRPath = callCommand $
  [iii|
  openssl req -new
  -key #{inputKeyPath}
  -out #{outputCSRPath}
  -subj #{defaultTLSSubject}
  -addext #{defaultSAN}
  |]

signTLSCSR :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
signTLSCSR inputCSRPath outputCertPath inputRootKeyPath inputRootCertPath = callCommand $
  [iii|
  openssl x509 -req
  -in #{inputCSRPath}
  -CA #{inputRootCertPath}
  -CAkey #{inputRootKeyPath}
  -out #{outputCertPath}
  -days 360 -sha256
  |] 

-- TODO: error handling when im not a lazy fucking chud
-- PKI not provided 
initPKI :: IO ()
initPKI = do
  unlessM (doesDirectoryExist certStoreDir) (createDirectory certStoreDir)
  unlessM (doesDirectoryExist tlsPKIDir) (createDirectory tlsPKIDir)

  createECKey curveConfig rootKeyPath
  createSelfSignedCert rootKeyPath rootCertPath

  createECKey  curveConfig  tlsKeyPath
  createTLSCSR tlsKeyPath   tlsCSRPath
  signTLSCSR   tlsCSRPath   tlsCertPath rootKeyPath rootCertPath


verifyPKI :: String -> IO [FailedReason]
verifyPKI serviceID = do
  certStoreMaybe <- readCertificateStore certStoreDir
  certStore      <- case certStoreMaybe of
                      Just cs   -> return cs
                      otherwise -> error $ "[!] Unable to parse or find certs in " ++ certStoreDir
  cert           <- CertificateChain <$> readSignedObject tlsCertPath


  validate HashSHA256
           defaultHooks
           defaultChecks
           certStore
           defaultValidationCache
           (serviceID, BS.empty)
           cert 
  
  


printReasons :: FailedReason -> IO ()
printReasons reason = putStrLn $ "[!] Check failed: " ++ (show reason)

printAllReasons :: [FailedReason] -> IO ()
printAllReasons reasons = for_ reasons printReasons



data TransportCertStatus = KeyMissing | CertMissing | KeyAndCertMissing | TransOkay | TransCheckError

chkTransportCertAndKey :: IO TransportCertStatus
chkTransportCertAndKey = do
  keyStatus  <- doesFileExist tlsKeyPath
  certStatus <- doesFileExist tlsCertPath
  if keyStatus && certStatus              then return TransOkay
  else if not keyStatus && not certStatus then return KeyAndCertMissing
  else if not keyStatus                   then return KeyMissing
  else if not certStatus                  then return CertMissing
  else return TransCheckError

