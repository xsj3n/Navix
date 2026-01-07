{-# LANGUAGE OverloadedStrings #-}

module Navix.DatabaseInterface
  ( getDatabaseConnection
  , registerMachineQuery
  , DatabaseConnection 
  ) where

import Database.PostgreSQL.Simple
import Data.String (fromString)
import Data.Int (Int64)
import Control.Monad (void, unless)

type DatabaseConnection = Connection

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

--- Run under postgres DB
checkForDatabaseQuery :: Query
checkForDatabaseQuery = "select exists (select 1 from pg_database where datname = 'managed');"

createDatabaseQuery :: Query
createDatabaseQuery = "create database managed"

--- Run under nixmachines DB
createMachinesTableQuery :: Query
createMachinesTableQuery = fromString $ unlines [
  "create table if not exists nixmachines(",
  "  serial_number text primary key,",
  "  mac_addr text not null,",
  "  public_key text not null",
  ");"
  ]

registerMachineQuery :: Query
registerMachineQuery = "insert into nixmachines (serial_number, mac_addr, public_key) values (?, ?, ?);"

-- Create db and default table if db is absent, returns used connection 
getDatabaseConnection :: IO Connection
getDatabaseConnection = do
  superUserConnection <- getSuperUserConnection
  putStrLn "[*] Connected to database as superuser for database check"
  [Only (result :: Bool)] <- query_ superUserConnection checkForDatabaseQuery
  unless result $ void (execute_ superUserConnection createDatabaseQuery :: IO Int64)
  close superUserConnection
  databaseConnection <- getConnection
  unless result $ void (execute_ databaseConnection createMachinesTableQuery :: IO Int64)
  return databaseConnection 
