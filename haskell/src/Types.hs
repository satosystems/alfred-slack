{-# LANGUAGE TemplateHaskell #-}

module Types
  ( (+++)
  , Cursor
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
import qualified Data.Text as T

(+++) :: T.Text -> T.Text -> T.Text
{-# NOINLINE [2] (+++) #-}
  -- Give time for the RULEs for (++) to fire in InitialPhase
  -- It's recursive, so won't inline anyway,
  -- but saying so is more explicit
(+++) = T.append

type UserId = T.Text

type TeamId = T.Text

type Token = T.Text

type Path = String

type URL = T.Text

type Cursor = T.Text

newtype ImagePath =
  ImagePath
    { path :: FilePath
    }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''ImagePath)

data Item =
  Item
    { uid :: T.Text
    , title :: T.Text
    , subtitle :: T.Text
    , arg :: Maybe T.Text
    -- , autocomplete :: T.Text
    , icon :: Maybe ImagePath
    }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Item)

data Variables =
  Variables
    { oldResults :: T.Text
    , oldArgv :: T.Text
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
