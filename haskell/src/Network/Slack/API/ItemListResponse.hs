{-# LANGUAGE TemplateHaskell #-}

module Network.Slack.API.ItemListResponse
  ( Purpose(..)
  , Channel(..)
  , Profile(..)
  , Member(..)
  , ResponseMetadata(..)
  , ListResponse(..)
  ) where

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH

import Types

data Profile =
  Profile
    { profileRealName :: String
    , profileDisplayName :: String
    , profileImage_48 :: URL
    }
  deriving (Read, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = snakeCase . drop 7} ''Profile)

-- makeLenses ''Profile
data Member =
  Member
    { memberId :: UserId
    , memberTeamId :: TeamId
    , memberName :: String
    , memberDeleted :: Bool
    , memberProfile :: Profile
    , memberIsBot :: Bool
    }
  deriving (Read, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = snakeCase . drop 6} ''Member)

-- makeLenses ''Member
newtype Purpose =
  Purpose
    { purposeValue :: String
    }
  deriving (Read, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = snakeCase . drop 7} ''Purpose)

-- makeLenses ''Purpose
newtype ResponseMetadata =
  ResponseMetadata
    { responseMetadataNextCursor :: String
    }
  deriving (Read, Show)

$(deriveJSON
    defaultOptions {fieldLabelModifier = snakeCase . drop 16}
    ''ResponseMetadata)

-- makeLenses ''ResponseMetadata
data Channel =
  Channel
    { channelId :: String
    , channelName :: String
    , channelIsArchived :: Bool
    , channelContextTeamId :: String
    , channelPurpose :: Purpose
    }
  deriving (Read, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = snakeCase . drop 7} ''Channel)

-- makeLenses ''Channel
data ListResponse =
  ListResponse
    { listResponseOk :: Bool
    , listResponseChannels :: Maybe [Channel]
    , listResponseMembers :: Maybe [Member]
    , listResponseResponseMetadata :: Maybe ResponseMetadata -- FIXME
    }
  deriving (Read, Show)

$(deriveJSON
    defaultOptions {fieldLabelModifier = snakeCase . drop 12}
    ''ListResponse)
-- makeLenses ''ListResponse
