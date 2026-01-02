{-# LANGUAGE OverloadedStrings #-}

module DatabaseInterface where
import Database.PostgreSQL.Simple
import Data.String (fromString)

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

