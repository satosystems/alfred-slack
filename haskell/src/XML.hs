{-# LANGUAGE OverloadedStrings #-}

module XML
  ( getUserOAuthToken
  ) where

import Data.String.Conversions
import qualified Data.Text.Lazy.IO as LTIO
import System.Directory
import Text.XML
import Text.XML.Cursor

getUserOAuthToken :: IO (Maybe String)
getUserOAuthToken = do
  let configFile = "../alfred/prefs.plist"
  exists <- doesFileExist configFile
  if exists
    then do
      text <- LTIO.readFile configFile
      let cursor = fromDocument $ parseText_ def text
      let keys = cursor $// element "key" >=> following &// content
      if null keys
        then return Nothing
        else (return . Just . cs . head) keys
    else return Nothing
