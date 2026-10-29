module Navix.Poll where
import Network.Wai (Request, getRequestBodyChunk)
import Database.PostgreSQL.Simple (Connection)
import qualified Data.ByteString as BS
import qualified Network.TLS as TLS
import Data.X509.Validation (FailedReason, validate, defaultChecks, defaultHooks)
import Data.X509.CertificateStore (CertificateStore)
import Network.Wai.Handler.WarpTLS
import Data.X509



pollResponse :: Request -> Connection -> IO Bool
pollResponse req con = undefined 
