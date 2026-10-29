{-# LANGUAGE DeriveGeneric #-}

module Navix.DataSchema where
import GHC.Generics
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Data.Time (UTCTime)

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
  { id                  :: Int
  , deviceName          :: String 
  , assignedCert        :: String
  , hardwareFingerprint :: String
  , enabled             :: Bool
  , compliant           :: Bool
  , createdAt           :: UTCTime
  } deriving (Generic, Show)

instance FromJSON NixMachineEnrollment where
instance ToJSON   NixMachineEnrollment where

instance ToRow NixMachine where
  toRow (NixMachine _id deviceName assignedCert hardwareFingerprint _enabled _compliant _createdAt) =
    [ toField deviceName
    , toField assignedCert
    , toField hardwareFingerprint
    ]

instance FromRow NixMachine where
  fromRow = NixMachine <$> field <*> field <*> field <*> field <*> field <*> field <*> field

----------

data NixMachineEnrollment = NixMachineEnrollment
  { enrollmentToken :: String
  , machineDetails  :: NixMachine
  } deriving (Generic, Show)


instance FromJSON NixMachine where
instance ToJSON   NixMachine where

------------------------------


data TokenValidityInfo = TokenValidityInfo
  {
    expiresAt :: UTCTime,
    revokedAt :: Maybe UTCTime,
    usedAt    :: Maybe UTCTime
  } deriving (Generic, Show)

instance FromRow TokenValidityInfo where
  fromRow = TokenValidityInfo <$> field <*> field <*> field

------------------------------
getfieldNames :: (ToJSON a) => a -> [String]
getfieldNames x = case toJSON x of
  Object o -> map show (KM.keys o)
  _        -> []
