{-# LANGUAGE DeriveGeneric #-}

module Navix.DataSchema where
import GHC.Generics
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField



data PollResponse = PollResponse
  { wipe          :: Bool
  , rebuild       :: Bool
  , reboot        :: Bool 
  , script        :: Bool
  , rotateRootPwd :: Bool
  , rotateLUKS    :: Bool
  , malwareScan   :: Bool 
  } deriving (Generic, Show)


instance FromJSON PollResponse where
instance ToJSON   PollResponse where
instance ToRow PollResponse where
  toRow (PollResponse wipe rebuild reboot script rotateRootPwd rotateLUKS malwareScan) =
      [ toField wipe
      , toField rebuild
      , toField reboot
      , toField script
      , toField rotateRootPwd
      , toField rotateLUKS
      , toField malwareScan
      ]
instance FromRow PollResponse where
    fromRow = PollResponse <$> field <*> field <*> field <*> field <*> field <*> field <*> field
------------------------------
data NixMachine = NixMachine
  { assignedCert        :: String
  , hardwareFingerprint :: String
  } deriving (Generic, Show)

instance FromJSON NixMachineEnrollment where
instance ToJSON   NixMachineEnrollment where

instance ToRow NixMachine where
  toRow (NixMachine assignedCert hardwareFingerprint) =
    [ toField assignedCert
    , toField hardwareFingerprint
    ]

instance FromRow NixMachine where
  fromRow = NixMachine <$> field <*> field

----------

data NixMachineEnrollment = NixMachineEnrollment
  { enrollmentToken :: String
  , machineDetails  :: NixMachine
  } deriving (Generic, Show)


instance FromJSON NixMachine where
instance ToJSON   NixMachine where

------------------------------


getfieldNames :: (ToJSON a) => a -> [String]
getfieldNames x = case toJSON x of
  Object o -> map show (KM.keys o)
  _        -> []
