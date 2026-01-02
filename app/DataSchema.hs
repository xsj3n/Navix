{-# LANGUAGE DeriveGeneric #-}

module DataSchema where
import GHC.Generics
import Data.Aeson
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Data.ByteString


data NixMachineEnrollment = NixMachineEnrollment {
  enrollmentToken :: String, 
  enrollmentserialNumber :: String,
  enrollmentmacAddr :: String,
  enrollmentpublicKey :: String
} deriving (Generic, Show)

instance FromJSON NixMachineEnrollment where
instance ToJSON   NixMachineEnrollment where
------------------------------
data NixMachine = NixMachine {
  serialNumber :: String,
  macAddr :: String,
  publicKey :: String
} deriving (Generic, Show)


instance FromJSON NixMachine where
instance ToJSON   NixMachine where

instance FromRow NixMachine where
  fromRow = NixMachine <$> field <*> field <*> field

instance ToRow NixMachine where
  toRow machine =
    [ toField $ serialNumber machine
    , toField $ macAddr machine
    , toField $ publicKey machine
    ]
------------------------------

toNixMachine :: NixMachineEnrollment -> NixMachine
toNixMachine enrollmentObject = NixMachine
  { serialNumber = enrollmentToken enrollmentObject
  , macAddr      = enrollmentmacAddr enrollmentObject
  , publicKey    = enrollmentpublicKey enrollmentObject
  }

type Sha256 = ByteString
