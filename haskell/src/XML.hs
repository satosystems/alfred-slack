{-# LANGUAGE OverloadedStrings #-}

module XML
  ( getValue
  ) where

import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
import System.Directory
import Text.XML
import Text.XML.Cursor

getValue :: T.Text -> IO (Maybe String)
getValue name = do
  let configFile = "../alfred/prefs.plist"
  exists <- doesFileExist configFile
  if exists
    then do
      text <- LTIO.readFile configFile
      let cursor = fromDocument $ parseText_ def text
      let keys =
            cursor $// element "key" >=>
            checkElement (\x -> elementNodes x == [NodeContent name]) >=>
            following &// content
      if null keys
        then return Nothing
        else (return . Just . cs . head) keys
    else return Nothing
