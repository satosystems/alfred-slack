{-# LANGUAGE OverloadedStrings #-}

module Alfred
  ( main'
  ) where

import Control.Concurrent.Async (async, wait)
import Control.Monad (void)
import Data.Aeson (encode)
import Data.List (sortOn)
import Data.List.Utils (uniq)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Data.Time.Clock.System
  ( SystemTime(systemNanoseconds, systemSeconds)
  , getSystemTime
  )
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import qualified System.IO.Strict as SIO
import System.Process (system)

import Slack
  ( cacheFile
  , clearChannelsCache
  , clearMembersCache
  , getChannels
  , getMembers
  , searchMessages
  )
import Types
  ( Item(..)
  , SearchResult(SearchResult, items, skipknowledge, variables)
  , Variables(Variables, oldArgv, oldResults)
  )
import qualified XML

usedArgsFilePath :: FilePath
usedArgsFilePath = cacheFile ".usedArgs"

loadUsedArgs :: IO [T.Text]
loadUsedArgs = do
  exists <- doesFileExist usedArgsFilePath
  if exists
    then do
      contents <- SIO.readFile usedArgsFilePath
      let usedArg = read contents
      return usedArg
    else return []

addUsedArg :: T.Text -> IO ()
addUsedArg arg' = do
  usedArg <- loadUsedArgs
  mNumberOfSelectedCaches <- XML.getValue "number_of_selected_caches"
  let numberOfSelectedCaches = maybe 20 (read . cs) mNumberOfSelectedCaches
  let newUsedArgs = take numberOfSelectedCaches . uniq $ arg' : usedArg
  writeFile usedArgsFilePath $ show newUsedArgs

sortUsedArgsFirst :: [Item] -> [T.Text] -> [Item]
sortUsedArgsFirst items' usedArgs =
  go [] (zip (map (T.tail . T.init . fromMaybe "\"\"" . arg) items') items') $
  reverse usedArgs
  where
    go :: [Item] -> [(T.Text, Item)] -> [T.Text] -> [Item]
    go hit base [] = uniq $ hit ++ map snd base
    go hit base (x:xs) =
      case x `lookup` base of
        Nothing -> go hit base xs
        Just item -> go (item : hit) base xs

open :: T.Text -> IO ()
open url = do
  _ <- system $ "open '" ++ cs url ++ "'"
  return ()

sortItemsByTitle :: [Item] -> [Item]
sortItemsByTitle = sortOn title

main' :: [T.Text] -> IO ()
main' args = do
  case head args of
    "open" ->
      let arg' = args !! 1
       in do open arg'
             addUsedArg arg'
    "search" -> do
      mToken <- XML.getValue "user_oauth_token"
      case (mToken, args !! 1) of
        (Nothing, _) ->
          print
            [ Item
                { uid = ""
                , title = "Oops!"
                , subtitle = "Please set User OAuth Token to config."
                , arg = Nothing
                , icon = Nothing
                }
            ]
        (Just token, "--update") -> do
          t1 <- getSystemTime
          a1 <- async clearChannelsCache
          a2 <- async clearMembersCache
          mapM_ wait [a1, a2]
          a3 <- async $ void $ getChannels token []
          a4 <- async $ void $ getMembers token []
          mapM_ wait [a3, a4]
          t2 <- getSystemTime
          let seconds = systemSeconds t2 - systemSeconds t1
          let nanoseconds = systemNanoseconds t2 - systemNanoseconds t1
          let (seconds', nanoseconds') =
                if nanoseconds < 0
                  then (seconds - 1, abs nanoseconds)
                  else (seconds, nanoseconds)
          putStrLn $
            cs $
            encode $
            SearchResult
              { skipknowledge = True
              , variables = Variables {oldResults = "[]", oldArgv = "[]"}
              , items =
                  [ Item
                      { uid = ""
                      , title = "Done."
                      , subtitle =
                          cs $
                          show seconds' ++
                          "." ++ take 2 (show nanoseconds') ++ " sec"
                      , arg = Nothing
                      , icon = Nothing
                      }
                  ]
              }
        (Just token, _) -> do
          let keywords = tail args
          a1 <- async $ getChannels token keywords
          a2 <- async $ getMembers token keywords
          channels <- wait a1
          members <- wait a2
          items' <-
            if null channels && null members && length keywords == 1
              then searchMessages token $ head keywords
              else return $
                   sortItemsByTitle members ++ sortItemsByTitle channels -- Note: There are so many channels, so I'll prioritize members.
          let items'' =
                if null items'
                  then [ Item
                           { uid = ""
                           , title = "NO MATCH."
                           , subtitle = "Please change keyword."
                           , arg = Nothing
                           , icon = Nothing
                           }
                       ]
                  else items'
          usedArgs <- loadUsedArgs
          let sortedItems = sortUsedArgsFirst items'' usedArgs
          putStrLn $
            cs $
            encode $
            SearchResult
              { skipknowledge = True
              , variables = Variables {oldResults = "[]", oldArgv = "[]"}
              , items = sortedItems
              }
    _ -> exitFailure
