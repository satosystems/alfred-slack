{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Alfred
  ( main'
  ) where

import Control.Concurrent.Async (async, wait)
import Control.Monad (void)
import Data.Aeson (encode)
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.List.Utils (uniq)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Data.Time.Clock.System
  ( SystemTime(systemNanoseconds, systemSeconds)
  , getSystemTime
  )
import Network.URI
  ( URIAuth(..)
  , escapeURIString
  , isUnreserved
  , parseURI
  , unEscapeString
  , uriAuthority
  , uriFragment
  , uriPath
  , uriQuery
  , uriRegName
  , uriScheme
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
  ( Cursor
  , ImagePath(..)
  , Item(..)
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

test :: IO ()
test = do
  let uriString = "http://user:password@www.example.com:80/path?query#fragment"
  case parseURI uriString of
    Nothing -> putStrLn "Invalid URI"
    Just uri -> do
      putStrLn $ "Scheme: " ++ uriScheme uri
      putStrLn $ "Authority: " ++ show (uriAuthority uri)
      putStrLn $ "Path: " ++ uriPath uri
      putStrLn $ "Query: " ++ uriQuery uri
      putStrLn $ "Fragment: " ++ uriFragment uri
      case uriAuthority uri of
        Nothing -> putStrLn "No authority component"
        Just auth -> do
          putStrLn $ "Userinfo: " ++ uriUserInfo auth
          putStrLn $ "Host: " ++ uriRegName auth
          putStrLn $ "Port: " ++ uriPort auth

parseUrl :: T.Text -> (T.Text, T.Text, [(T.Text, T.Text)])
parseUrl url =
  let uri = (fromJust . parseURI . cs) url
   in ( (cs . uriScheme) uri
      , (cs . uriRegName . fromJust . uriAuthority) uri
      , (parseQuery . uriQuery) uri)
  where
    parseQuery :: String -> [(T.Text, T.Text)]
    parseQuery query =
      let cleanQuery = T.dropWhile (== '?') $ cs query
          keyValues = T.splitOn "&" cleanQuery
       in map parseKeyValue keyValues
    parseKeyValue :: T.Text -> (T.Text, T.Text)
    parseKeyValue kv =
      let (key, value) = T.breakOn "=" kv
       in (key, (cs . unEscapeString . cs . T.drop 1) value)

open :: T.Text -> IO ()
open url = do
  case parseUrl url of
    ("slack:", _, _) -> do
      _ <- system $ "open '" ++ cs url ++ "'"
      return ()
    ("alfred-slack:", "command", query) -> do
      let command = (cs . fromJust . lookup "q") query
      _ <- system command
      return ()
    ("alfred-slack:", "next", query) -> do
      let q = (read . cs . fromJust . lookup "q") query :: [T.Text]
      let cursor = (cs . fromJust . lookup "cursor") query
      main' $ "next" : cursor : q
    _ -> do
      return ()

sortItemsByTitle :: [Item] -> [Item]
sortItemsByTitle = sortOn title

output :: [T.Text] -> [Item] -> Cursor -> IO ()
output keywords resultItems cursor =
  putStrLn $
  cs $
  encode $
  SearchResult
    { skipknowledge = True
    , variables = Variables {oldResults = "[]", oldArgv = "[]"}
    , items =
        resultItems ++
        ([ Item
           ""
           "Next..."
           "Show next results"
           (Just $
            cs $
            "alfred-slack://next?q=" ++
            escapeURIString isUnreserved (show keywords) ++
            "&cursor=" ++ escapeURIString isUnreserved (cs cursor))
           Nothing
         | (not . T.null) cursor
         ])
    }

main' :: [T.Text] -> IO ()
main' args = do
  case head args of
    "open" ->
      let arg' = args !! 1
       in do open arg'
             addUsedArg arg'
    "next" -> do
      let cursor = args !! 1
      let keywords = drop 2 args
      token <- XML.getValue "user_oauth_token" <&> fromJust
      (nextCursor, resultItems) <-
        searchMessages token cursor (T.intercalate " " keywords)
      output keywords resultItems nextCursor
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
          let lastKeyword = last keywords
          a1 <- async $ getChannels token keywords
          a2 <- async $ getMembers token keywords
          channels <- wait a1
          members <- wait a2
          let (prefix, getter, fn) =
                case ( "in:#" `T.isPrefixOf` lastKeyword
                     , "from:@" `T.isPrefixOf` lastKeyword) of
                  (True, _) -> ("in:#", Just getChannels, Just $ cs . title)
                  (_, True) ->
                    ( "from:@"
                    , Just getMembers
                    , Just $
                      (cs . last . init . T.splitOn ")") .
                      (last . T.splitOn "(") . subtitle)
                  _ -> ("", Nothing, Nothing)
          candidateItems <-
            if isJust fn
              then do
                let partOfItemName = T.drop (length prefix) lastKeyword
                a3 <- async $ fromJust getter token [partOfItemName]
                candidateItems <- wait a3
                let searchText = T.intercalate " " $ "" : init keywords
                return $
                  map
                    (\item ->
                       item
                         { arg =
                             Just $
                             cs $
                             "alfred-slack://command?q=" ++
                             escapeURIString
                               isUnreserved
                               ("/usr/bin/osascript -e 'tell application \"Alfred 5\" to search \"ss" ++
                                cs searchText ++
                                " " ++ prefix ++ fromJust fn item ++ "\"'")
                         , icon = Just $ ImagePath "./alfred.png"
                         })
                    (filter
                       (\item -> partOfItemName /= title item)
                       candidateItems)
              else return []
          (cursor, items') <-
            if null channels && null members
              then searchMessages token "*" (T.intercalate " " keywords)
              else return
                     ("", sortItemsByTitle members ++ sortItemsByTitle channels)
                   -- Note: There are so many channels, so I'll prioritize members.
          let items'' = items' ++ candidateItems
          let items''' =
                if null items''
                  then [ Item
                           { uid = ""
                           , title = "NO MATCH."
                           , subtitle = "Please change keyword."
                           , arg = Nothing
                           , icon = Nothing
                           }
                       ]
                  else items''
          usedArgs <- loadUsedArgs
          let sortedItems = sortUsedArgsFirst items''' usedArgs
          output keywords sortedItems cursor
    _ -> exitFailure
