{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Navix.PKI where

import System.Process (callCommand)
import Data.String.Interpolate
import System.Directory (doesFileExist)
import Data.X509
import Data.X509.Validation
import Data.X509.CertificateStore

rootCertPath :: FilePath
rootCertPath = "./cert-store/root-crt.pem"

rootKeyPath :: FilePath
rootKeyPath = "./cert-store/root-key.pem"

tlsCertPath :: FilePath
tlsCertPath = "./cert-store/transport/tls-crt.pem"

tlsKeyPath :: FilePath
tlsKeyPath = "./cert-store/transport/tls-key.pem"

tlsCSRPath :: FilePath
tlsCSRPath = "./cert-store/transport/tls-csr.pem"

defaultRootSubject :: String
defaultRootSubject = "'/C=/ST=/L=/O=/CN=Navix'"

defaultTLSSubject :: String
defaultTLSSubject = "'/C=/ST=/L=/O=/CN=Navix.local'"

type CurveType = String 
curveConfig :: String
curveConfig = "prime256v1"


createECKey :: CurveType -> FilePath -> IO ()
createECKey curve path = callCommand $
  [i|openssl ecparam -name #{curve} -genkey -noout -out #{path}|]


createSelfSignedCert :: FilePath -> FilePath -> IO ()
createSelfSignedCert inputKeyPath outputCertPath= callCommand $
  [iii|
  openssl req -x509 -new -nodes -sha256 -days 3650
  -key #{inputKeyPath}
  -out #{outputCertPath}
  -subj #{defaultRootSubject}
  |]

createTLSCSR :: FilePath -> FilePath -> IO ()
createTLSCSR inputKeyPath outputCSRPath = callCommand $
  [i|openssl req -new -key #{inputKeyPath} -out #{outputCSRPath} -subj #{defaultTLSSubject}|]

signTLSCSR :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
signTLSCSR inputCSRPath outputCertPath inputRootKeyPath inputRootCertPath = callCommand $
  [iii|
  openssl x509 -req
  -in #{inputCSRPath}
  -CA #{inputRootCertPath}
  -CAKey #{inputRootKeyPath}
  -out #{outputCertPath}
  -CAcreateserial -days 3650 -sha256
  -addext "subjectAltName = DNS:Navix.local,DNS:www.Navix.local"
  |] 

-- TODO: error handling when im not a lazy fucking chud
-- PKI not provided 
initPKI :: IO ()
initPKI = do
  createECKey curveConfig rootKeyPath
  createSelfSignedCert rootKeyPath rootCertPath

  createECKey  curveConfig tlsKeyPath
  createTLSCSR tlsKeyPath   tlsCSRPath
  signTLSCSR   tlsCSRPath   tlsCertPath rootKeyPath rootCertPath
  

verifyPKI :: undefined
verifyPKI = undefined



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

