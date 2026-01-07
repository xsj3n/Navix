module Navix.PollResponse where
import Network.Wai (Request, getRequestBodyChunk)
import Database.PostgreSQL.Simple (Connection)
import qualified Data.ByteString as BS
import Navix.DataSchema (Sha256)


