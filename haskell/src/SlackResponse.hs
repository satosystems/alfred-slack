{-# LANGUAGE TemplateHaskell #-}

module SlackResponse
  ( Purpose(..)
  , Channel(..)
  , Profile(..)
  , Member(..)
  , ResponseMetadata(..)
  , ListResponse(..)
  , Messages(..)
  , Match(..)
  , MatchChannel(..)
  , Pagination(..)
  , Paging(..)
  ) where

import Data.Aeson (Options(fieldLabelModifier), defaultOptions)
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Text as T

import Types (TeamId, URL, UserId)

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

data MatchChannel =
  MatchChannel
    { matchChannelId :: T.Text
    , matchChannelIsChannel :: Bool
    , matchChannelIsGroup :: Bool
    , matchChannelIsMpim :: Bool
    , matchChannelName :: T.Text
    }
  deriving (Read, Show)

$(deriveJSON
    defaultOptions {fieldLabelModifier = snakeCase . drop 12}
    ''MatchChannel)

data Match =
  Match
    { matchIid :: T.Text
    , matchTeam :: T.Text
    , matchChannel :: MatchChannel
    , matchUsername :: T.Text
    , matchTs :: T.Text
    , matchText :: T.Text
    , matchPermalink :: T.Text
    }
  deriving (Read, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = snakeCase . drop 5} ''Match)

data Pagination =
  Pagination
    { paginationTotalCount :: Int
    , paginationPerPage :: Int
    , paginationNextCursor :: T.Text
    , paginationFirst :: Int
    , paginationLast :: Int
    }
  deriving (Read, Show)

$(deriveJSON
    defaultOptions {fieldLabelModifier = snakeCase . drop 10}
    ''Pagination)

data Paging =
  Paging
    { pagingCount :: Int
    , pagingTotal :: Int
    , pagingNextCursor :: T.Text
    }
  deriving (Read, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = snakeCase . drop 6} ''Paging)

data Messages =
  Messages
    { messagesMatches :: [Match]
    , messagesPagination :: Pagination
    , messagesPaging :: Paging
    , messagesTotal :: Int
    }
  deriving (Read, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = snakeCase . drop 8} ''Messages)

data ListResponse =
  ListResponse
    { listResponseOk :: Bool
    , listResponseChannels :: Maybe [Channel]
    , listResponseMembers :: Maybe [Member]
    , listResponseMessages :: Maybe Messages
    , listResponseResponseMetadata :: Maybe ResponseMetadata
    }
  deriving (Read, Show)

$(deriveJSON
    defaultOptions {fieldLabelModifier = snakeCase . drop 12}
    ''ListResponse)
