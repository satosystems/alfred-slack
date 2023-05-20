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
import qualified Data.Text as T

import Types

data Profile =
  Profile
    { profileRealName :: T.Text
    , profileDisplayName :: T.Text
    , profileImage_48 :: URL
    }
  deriving (Read, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = snakeCase . drop 7} ''Profile)

data Member =
  Member
    { memberId :: UserId
    , memberTeamId :: TeamId
    , memberName :: T.Text
    , memberDeleted :: Bool
    , memberProfile :: Profile
    , memberIsBot :: Bool
    }
  deriving (Read, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = snakeCase . drop 6} ''Member)

newtype Purpose =
  Purpose
    { purposeValue :: T.Text
    }
  deriving (Read, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = snakeCase . drop 7} ''Purpose)

newtype ResponseMetadata =
  ResponseMetadata
    { responseMetadataNextCursor :: T.Text
    }
  deriving (Read, Show)

$(deriveJSON
    defaultOptions {fieldLabelModifier = snakeCase . drop 16}
    ''ResponseMetadata)

data Channel =
  Channel
    { channelId :: T.Text
    , channelName :: T.Text
    , channelIsArchived :: Bool
    , channelContextTeamId :: T.Text
    , channelPurpose :: Purpose
    }
  deriving (Read, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = snakeCase . drop 7} ''Channel)

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
