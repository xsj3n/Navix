module Navix.TLS where
import qualified Data.ByteString as BS
import qualified Network.TLS as TLS
import Data.X509.Validation (FailedReason, validate, defaultChecks, defaultHooks)
import Data.X509.CertificateStore (CertificateStore)
import Network.Wai.Handler.WarpTLS
import Network.Wai.Handler.WarpTLS.Internal (certSettings, CertSettings(CertFromFile))
import Data.X509




-- For now we will just accept, later on we can actually implement the mutual TLS certificate 
-- logic. This seems mildly annoying and I would rather do the networking first
validateTLSCached :: CertificateStore -> String -> (TLS.CertificateChain -> IO [FailedReason])
validateTLSCached certStore serviceID = (\cert -> validate  HashSHA256
                                                            defaultHooks
                                                            defaultChecks
                                                            certStore
                                                            TLS.defaultValidationCache
                                                            (serviceID, BS.empty)
                                                            cert 
                                        ) 

mutualTLSSettings :: FilePath -> FilePath -> TLSSettings
mutualTLSSettings cert key = defaultTlsSettings
  { certSettings = CertFromFile cert [] key 
  , tlsWantClientCert = True
  , tlsServerHooks = TLS.defaultServerHooks { TLS.onClientCertificate = onClientCertificateMutalTLS }
  }



onClientCertificateMutalTLS :: TLS.CertificateChain -> IO TLS.CertificateUsage
onClientCertificateMutalTLS cert = pure TLS.CertificateUsageAccept

