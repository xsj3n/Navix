{-# LANGUAGE OverloadedStrings #-}

module Navix.DatabaseInterface
  ( getDatabaseConnection
  , getRegistrationQuery
  , readQueryFile
  , readQueryFiles
  ) where

import Data.Typeable
import Database.PostgreSQL.Simple
import Data.String.Interpolate
import Data.String (fromString)
import Data.Int (Int64)
import Control.Monad (void, unless)
import Generic.Data (gdatatypeName)
import Navix.DataSchema (NixMachine (NixMachine), getfieldNames)
import System.Directory.Extra (listDirectories)
import Data.List.Extra (splitOn, trim)
import Data.Foldable (for_)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Crypto.Hash (Digest)
import Data.ByteString (ByteString)


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
insertToken :: Connection -> ByteString  -> IO Int64
insertToken con tokenHash = do
  query <- insertEnrollmentTokenQuery
  execute con query $ Only $ Binary tokenHash

getTokenInfo :: (ToRow r) => Connection -> ByteString -> IO [r] 
getTokenInfo con hash = 

validateToken :: Connection -> ByteString -> IO Int64
validateToken con tokenHash = do
  beginTransaction con
  
  pure 0
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
