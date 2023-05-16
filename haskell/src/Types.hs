{-# LANGUAGE TemplateHaskell #-}

module Types
  ( Cursor
  , UserId
  , TeamId
  , Token
  , Path
  , URL
  , Item(..)
  , ImagePath(..)
  , SearchResult(..)
  , Variables(..)
  ) where

import Data.Aeson
import Data.Aeson.TH

type UserId = String

type TeamId = String

type Token = String

type Path = String

type URL = String

type Cursor = String

newtype ImagePath =
  ImagePath
    { path :: FilePath
    }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''ImagePath)

data Item =
  Item
    { uid :: String
    , title :: String
    , subtitle :: String
    , arg :: String
    -- , autocomplete :: String
    , icon :: Maybe ImagePath
    }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Item)

data Variables =
  Variables
    { oldResults :: String
    , oldArgv :: String
    }
  deriving (Show)

$(deriveJSON defaultOptions ''Variables)

data SearchResult =
  SearchResult
    { skipknowledge :: Bool
    , variables :: Variables
    , items :: [Item]
    }
  deriving (Show)

$(deriveJSON defaultOptions ''SearchResult)
