{-# LANGUAGE OverloadedStrings #-}
-- this file is claude slop bc i didnt want to write ts 
module Navix.Configuration
  ( NavixConfiguration (..)
  , ServerConfig (..)
  , DatabaseConfig (..)
  , PKIConfig (..)
  , defaultConfiguration
  , loadConfiguration
  ) where

import qualified Toml
import Toml (TomlCodec, (.=))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Directory (doesFileExist)

data ServerConfig = ServerConfig
  { serverPort :: Int
  } deriving (Eq, Show)

data DatabaseConfig = DatabaseConfig
  { databaseHost :: String
  , databaseUser :: String
  , databasePort :: Int
  , databaseName :: String
  } deriving (Eq, Show)

data PKIConfig = PKIConfig
  { pkiCertStoreDir         :: FilePath
  , pkiCommonName           :: String
  , pkiCurve                :: String
  , pkiRootCertValidityDays :: Int
  , pkiTlsCertValidityDays  :: Int
  } deriving (Eq, Show)

data NavixConfiguration = NavixConfiguration
  { serverConfig   :: ServerConfig
  , databaseConfig :: DatabaseConfig
  , pkiConfig      :: PKIConfig
  } deriving (Eq, Show)

-- what's already hardcoded elsewhere today, kept here as the fallback
-- for whatever a config file leaves unset
defaultConfiguration :: NavixConfiguration
defaultConfiguration = NavixConfiguration
  { serverConfig = ServerConfig
      { serverPort = 3000
      }
  , databaseConfig = DatabaseConfig
      { databaseHost = "localhost"
      , databaseUser = "xs"
      , databasePort = 5432
      , databaseName = "managed"
      }
  , pkiConfig = PKIConfig
      { pkiCertStoreDir         = "./cert-store/"
      , pkiCommonName           = "Navix.local"
      , pkiCurve                = "prime256v1"
      , pkiRootCertValidityDays = 3650
      , pkiTlsCertValidityDays  = 360
      }
  }

-- mirrors NavixConfiguration field-for-field, but every field optional,
-- so an admin's config file only has to mention what it wants to override
data PartialServerConfig = PartialServerConfig
  { partialServerPort :: Maybe Int
  }

data PartialDatabaseConfig = PartialDatabaseConfig
  { partialDatabaseHost :: Maybe String
  , partialDatabaseUser :: Maybe String
  , partialDatabasePort :: Maybe Int
  , partialDatabaseName :: Maybe String
  }

data PartialPKIConfig = PartialPKIConfig
  { partialPkiCertStoreDir         :: Maybe FilePath
  , partialPkiCommonName           :: Maybe String
  , partialPkiCurve                :: Maybe String
  , partialPkiRootCertValidityDays :: Maybe Int
  , partialPkiTlsCertValidityDays  :: Maybe Int
  }

data PartialConfiguration = PartialConfiguration
  { partialServerConfig   :: PartialServerConfig
  , partialDatabaseConfig :: PartialDatabaseConfig
  , partialPkiConfig      :: PartialPKIConfig
  }

serverCodec :: TomlCodec PartialServerConfig
serverCodec = PartialServerConfig
  <$> Toml.dioptional (Toml.int "port") .= partialServerPort

databaseCodec :: TomlCodec PartialDatabaseConfig
databaseCodec = PartialDatabaseConfig
  <$> Toml.dioptional (Toml.string "host") .= partialDatabaseHost
  <*> Toml.dioptional (Toml.string "user") .= partialDatabaseUser
  <*> Toml.dioptional (Toml.int    "port") .= partialDatabasePort
  <*> Toml.dioptional (Toml.string "name") .= partialDatabaseName

pkiCodec :: TomlCodec PartialPKIConfig
pkiCodec = PartialPKIConfig
  <$> Toml.dioptional (Toml.string "cert_store_dir")          .= partialPkiCertStoreDir
  <*> Toml.dioptional (Toml.string "common_name")             .= partialPkiCommonName
  <*> Toml.dioptional (Toml.string "curve")                   .= partialPkiCurve
  <*> Toml.dioptional (Toml.int    "root_cert_validity_days") .= partialPkiRootCertValidityDays
  <*> Toml.dioptional (Toml.int    "tls_cert_validity_days")  .= partialPkiTlsCertValidityDays

configCodec :: TomlCodec PartialConfiguration
configCodec = PartialConfiguration
  <$> Toml.table serverCodec   "server"   .= partialServerConfig
  <*> Toml.table databaseCodec "database" .= partialDatabaseConfig
  <*> Toml.table pkiCodec      "pki"      .= partialPkiConfig

applyDefaults :: PartialConfiguration -> NavixConfiguration
applyDefaults partial = NavixConfiguration
  { serverConfig = ServerConfig
      { serverPort = fromMaybe (serverPort defServer) (partialServerPort partServer)
      }
  , databaseConfig = DatabaseConfig
      { databaseHost = fromMaybe (databaseHost defDatabase) (partialDatabaseHost partDatabase)
      , databaseUser = fromMaybe (databaseUser defDatabase) (partialDatabaseUser partDatabase)
      , databasePort = fromMaybe (databasePort defDatabase) (partialDatabasePort partDatabase)
      , databaseName = fromMaybe (databaseName defDatabase) (partialDatabaseName partDatabase)
      }
  , pkiConfig = PKIConfig
      { pkiCertStoreDir         = fromMaybe (pkiCertStoreDir defPki) (partialPkiCertStoreDir partPki)
      , pkiCommonName           = fromMaybe (pkiCommonName defPki) (partialPkiCommonName partPki)
      , pkiCurve                = fromMaybe (pkiCurve defPki) (partialPkiCurve partPki)
      , pkiRootCertValidityDays = fromMaybe (pkiRootCertValidityDays defPki) (partialPkiRootCertValidityDays partPki)
      , pkiTlsCertValidityDays  = fromMaybe (pkiTlsCertValidityDays defPki) (partialPkiTlsCertValidityDays partPki)
      }
  }
  where
    defServer   = serverConfig defaultConfiguration
    partServer  = partialServerConfig partial
    defDatabase = databaseConfig defaultConfiguration
    partDatabase = partialDatabaseConfig partial
    defPki      = pkiConfig defaultConfiguration
    partPki     = partialPkiConfig partial

loadConfiguration :: FilePath -> IO NavixConfiguration
loadConfiguration path = do
  exists <- doesFileExist path
  if not exists
    then return defaultConfiguration
    else do
      result <- Toml.decodeFileEither configCodec path
      case result of
        Right partial -> return $ applyDefaults partial
        Left errs     -> error $ "[!] Failed to parse config file " ++ path ++ ":\n"
                               ++ T.unpack (Toml.prettyTomlDecodeErrors errs)
