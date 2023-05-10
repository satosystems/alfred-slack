{-# LANGUAGE TemplateHaskell #-}

module Types
  ( Cursor
  , UserId
  , TeamId
  , Token
  , Path
  , URL
  , Item(..)
  ) where

import Data.Aeson
import Data.Aeson.TH

type UserId = String

type TeamId = String

type Token = String

type Path = String

type URL = String

type Cursor = String

data Item =
  Item
    { uid :: String
    , title :: String
    , subtitle :: String
    , arg :: String
    -- , autocomplete :: String
    , icon :: Maybe FilePath
    }
  deriving (Show)

$(deriveJSON defaultOptions ''Item)
