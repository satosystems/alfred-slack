{-# LANGUAGE OverloadedStrings #-}

module XML
  ( getValue
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
import System.Directory
import Text.XML
import Text.XML.Cursor

getValue :: T.Text -> IO (Maybe T.Text)
getValue name = do
  let configFile = "../alfred/prefs.plist"
  exists <- doesFileExist configFile
  if exists
    then do
      text <- LTIO.readFile configFile
      let cursor = fromDocument $ parseText_ def text
      let values =
            cursor $// element "key" >=>
            checkElement (\x -> elementNodes x == [NodeContent name]) >=>
            following &// content
      if null values
        then return Nothing
        else (return . Just . head) values
    else return Nothing
