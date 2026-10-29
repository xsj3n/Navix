{-# LANGUAGE OverloadedStrings #-}

module Navix.DatabaseInterface
  ( withConnection
  , ConnectionPool
  , ConnectionType
  , DatabaseConnection
  , dbConn
  , createConnectionPool
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
import Control.Monad.State.Strict (runState, State, MonadState (put, get), StateT (runStateT), MonadIO (liftIO), evalStateT)
import Control.Monad.Extra (replicateM)
import GHC.Conc (TVar)
import GHC.Conc.Sync (newTVarIO)
import Network.Wai (Request)
import Control.Concurrent.STM (modifyTVar')
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM (readTVar)
import Control.Concurrent.STM (writeTVar)
import Control.Concurrent.STM (retry)
import Control.Exception (bracket)



----
---- connection management
----
newtype ConnectionPool = ConnectionPool (TVar [DatabaseConnection])
data ConnectionType = Privileged | Unprivileged 
data DatabaseConnection = DatabaseConnection
  { id       :: Int
  , dbConn     :: Connection
  , dbConnType :: ConnectionType 
  }

getConnection :: Int -> IO DatabaseConnection
getConnection id = do
  dbConn <- connect getConnectionInfo
  return DatabaseConnection {id = id, dbConn = dbConn, dbConnType = Unprivileged }

poolBuilder :: StateT Int IO (DatabaseConnection)
poolBuilder = do
  count <- get
  con   <- liftIO (getConnection count)  
  put $ count + 1
  return con 
  
createConnectionPool :: Int -> IO ConnectionPool
createConnectionPool connCount = do
  conns <- evalStateT (replicateM connCount poolBuilder) 0
  ConnectionPool <$> newTVarIO conns

withConnection :: ConnectionPool -> Request -> (Request -> DatabaseConnection -> IO a) -> IO a
withConnection (ConnectionPool var) request action  =
  bracket acquire release (action request) 
  where
    acquire = atomically $ do
      conns <- readTVar var
      case conns of
        []     -> retry
        c:cs -> writeTVar var cs >> return c   
    release c = endTransaction c >> (atomically $ modifyTVar' var (c :))
  
getSuperUserConnection :: IO DatabaseConnection
getSuperUserConnection = do
  dbConn <- connect getSuperUserConnectionInfo
  return DatabaseConnection { id = 0, dbConn = dbConn, dbConnType = Privileged }


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


-- does some init stuff to make sure the db is ready 
initDatabase :: IO ()
initDatabase = do
  superUserConnection <- getSuperUserConnection
  putStrLn "[*] Connected to database as superuser for database check"
  [Only (result :: Bool)] <- query_ (dbConn superUserConnection) checkForDatabaseQuery
  unless result $ (void $ createDatabaseQuery >>= runQueries_ superUserConnection)
  close $ dbConn superUserConnection

----
---- enrollment token related
----
insertToken :: Connection -> ByteString  -> IO Int64
insertToken con tokenHash = do
  sQuery <- insertEnrollmentTokenQuery
  execute con sQuery $ Only $ Binary tokenHash


getTokenInfo :: Connection -> ByteString -> IO [TokenValidityInfo]
getTokenInfo con tokenHash = do
  sQuery <- getTokenInfoQuery
  query con sQuery $ Only $ Binary tokenHash

consumeToken :: Connection -> ByteString -> IO Int64
consumeToken con tokenHash = do
  sQuery <- consumeTokenQuery
  execute con sQuery $ Only $ Binary tokenHash


insertEnrollmentTokenQuery :: IO Query
insertEnrollmentTokenQuery = readQueryFile "./queries/registration/insert-token.sql" 

getTokenInfoQuery :: IO Query
getTokenInfoQuery = readQueryFile "./queries/registration/get-token-validation-info.sql"

consumeTokenQuery :: IO Query
consumeTokenQuery = readQueryFile "./queries/registration/consume-token.sql"

----
---- registration related
----
getRegistrationQuery :: IO Query
getRegistrationQuery = readQueryFile "./queries/register.sql"

checkForDatabaseQuery :: Query
checkForDatabaseQuery = "select exists (select 1 from pg_database where datname = 'managed');"

createDatabaseQuery :: IO [Query]
createDatabaseQuery = readQueryFiles "./queries/init/table-creation.sql"

----
---- sql query function helpers 
----

runQueries_ :: DatabaseConnection -> [Query] -> IO Int64
runQueries_ con queries = sum <$> mapM (execute_ $ dbConn con) queries 

-- transaction wrappers, may use them, but only if i need to use these within other modules,
-- which, to be fair, i shouldnt 
beginTransaction :: DatabaseConnection -> IO ()
beginTransaction con = begin $ dbConn con

endTransaction :: DatabaseConnection -> IO ()
endTransaction con = commit $ dbConn con

rollbackTransaction :: DatabaseConnection -> IO ()
rollbackTransaction con = rollback $ dbConn con

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


