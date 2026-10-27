{-# LANGUAGE OverloadedStrings #-}

module Navix.DatabaseInterface
  ( getDatabaseConnection
  , getRegistrationQuery
  , consumeToken
  , getTokenInfo
  , insertToken
  , readQueryFile
  , readQueryFiles
  ) where
import Navix.DataSchema (NixMachine (NixMachine), TokenValidityInfo (TokenValidityInfo))
import Data.Typeable
import Database.PostgreSQL.Simple
import Data.String.Interpolate
import Data.String (fromString)
import Data.Int (Int64)
import Control.Monad (void, unless)
import Generic.Data (gdatatypeName)
import System.Directory.Extra (listDirectories)
import Data.List.Extra (splitOn, trim)
import Data.Foldable (for_)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Crypto.Hash (Digest)
import Data.ByteString (ByteString)
import Data.Time (UTCTime)


getConnection :: IO Connection
getConnection = connect getConnectionInfo

getSuperUserConnection :: IO Connection
getSuperUserConnection = connect getSuperUserConnectionInfo


getConnectionInfo :: ConnectInfo
getConnectionInfo = defaultConnectInfo
  { connectHost     = "localhost"
  , connectDatabase = "managed"
  , connectUser     = "xs"
  , connectPort     = 5432 
  }


getSuperUserConnectionInfo :: ConnectInfo
getSuperUserConnectionInfo = defaultConnectInfo
  { connectHost     = "localhost"
  , connectDatabase = "postgres"
  , connectUser     = "xs"
  , connectPort     = 5432 
  }


-- does some init stuff then returns a connection
getDatabaseConnection :: IO Connection
getDatabaseConnection = do
  superUserConnection <- getSuperUserConnection
  putStrLn "[*] Connected to database as superuser for database check"
  [Only (result :: Bool)] <- query_ superUserConnection checkForDatabaseQuery
  unless result $ (void $ createDatabaseQuery >>= runQueries_ superUserConnection)
  close superUserConnection
  getConnection


-- enrollment related

---- this shouldnt fail unless the connection fails 
insertToken :: Connection -> ByteString  -> IO Int64
insertToken con tokenHash = do
  sQuery <- insertEnrollmentTokenQuery
  execute con sQuery $ Only $ Binary tokenHash


---- if [] len == 0 then the token is not valid 
getTokenInfo :: Connection -> ByteString -> IO [TokenValidityInfo]
getTokenInfo con tokenHash = do
  sQuery <- getTokenInfoQuery
  query con sQuery $ Only $ Binary tokenHash

consumeToken :: Connection -> ByteString -> IO Int64
consumeToken con tokenHash = do
  sQuery <- consumeTokenQuery
  execute con sQuery $ Only $ Binary tokenHash

--- sql query function helpers 
runQueries_ :: Connection -> [Query] -> IO Int64
runQueries_ con queries = sum <$> mapM (execute_ con) queries 

-- transaction wrappers, may use them, but only if i need to use these within other modules,
-- which, to be fair, i shouldnt 
beginTransaction :: Connection -> IO ()
beginTransaction con = begin con

endTransaction :: Connection -> IO ()
endTransaction con = commit con

rollbackTransaction :: Connection -> IO ()
rollbackTransaction con = rollback con

-- helper functions for reading sql queries out of files
-- preferable to keep sql & code logic separate 
readQueryFiles :: FilePath -> IO [Query]
readQueryFiles dir =  do
  paths <- fmap (dir </>) <$> listDirectory dir
  queryChunks <- sequenceA (readFile <$> paths)
  let queries = filter (not . null) $ map trim $ splitOn "--" $ concat queryChunks
  pure $ fromString <$> queries 

readQueryFile :: FilePath -> IO Query
readQueryFile path = fromString <$> readFile path 

getRegistrationQuery :: IO Query
getRegistrationQuery = readQueryFile "./queries/register.sql"

checkForDatabaseQuery :: Query
checkForDatabaseQuery = "select exists (select 1 from pg_database where datname = 'managed');"

createDatabaseQuery :: IO [Query]
createDatabaseQuery = readQueryFiles "./queries/init/table-creation.sql"

insertEnrollmentTokenQuery :: IO Query
insertEnrollmentTokenQuery = readQueryFile "./queries/registration/insert-token.sql" 

getTokenInfoQuery :: IO Query
getTokenInfoQuery = readQueryFile "./queries/registration/get-token-validation-info.sql"

consumeTokenQuery :: IO Query
consumeTokenQuery = readQueryFile "./queries/registration/consume-token.sql"
