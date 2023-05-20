{-# LANGUAGE OverloadedStrings #-}

module Alfred
  ( main'
  ) where

import Control.Concurrent.Async
import Control.Monad
import Data.Aeson
import Data.List.Utils
import Data.String.Conversions
import Data.Time.Clock.System
import Network.Slack.API.ItemList
import System.Directory
import System.Exit
import qualified System.IO.Strict as SIO
import System.Process

import Types
import qualified XML

usedArgsFilePath :: FilePath
usedArgsFilePath = cacheFile ".usedArgs"

loadUsedArgs :: IO [String]
loadUsedArgs = do
  exists <- doesFileExist usedArgsFilePath
  if exists
    then do
      contents <- SIO.readFile usedArgsFilePath
      let usedArg = read contents
      return usedArg
    else return []

addUsedArg :: String -> IO ()
addUsedArg arg = do
  usedArg <- loadUsedArgs
  let newUsedArgs = take 100 . uniq $ arg : usedArg
  writeFile usedArgsFilePath $ show newUsedArgs

sortUsedArgsFirst :: [Item] -> [String] -> [Item]
sortUsedArgsFirst items usedArgs =
  go [] (zip (map (tail . init . arg) items) items) $ reverse usedArgs
  where
    go :: [Item] -> [(String, Item)] -> [String] -> [Item]
    go hit base [] = uniq $ hit ++ map snd base
    go hit base (x:xs) =
      case x `lookup` base of
        Nothing -> go hit base xs
        Just item -> go (item : hit) base xs

open :: String -> IO ()
open url = do
  _ <- system $ "open '" ++ url ++ "'"
  return ()

main' :: [String] -> IO ()
main' args = do
  case head args of
    "open" ->
      let arg = args !! 1
       in do open arg
             addUsedArg arg
    "search" -> do
      mToken <- XML.getValue "user_oauth_token"
      case (mToken, args !! 1) of
        (Nothing, _) ->
          print
            [ Item
                { uid = ""
                , title = "Oops!"
                , subtitle = "Please set User OAuth Token to config."
                , arg = "''"
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
            convertString $
            encode $
            SearchResult
              { skipknowledge = True
              , variables = Variables {oldResults = "[]", oldArgv = "[]"}
              , items =
                  [ Item
                      { uid = ""
                      , title = "Done."
                      , subtitle =
                          show seconds' ++
                          "." ++ take 2 (show nanoseconds') ++ " sec"
                      , arg = ""
                      , icon = Nothing
                      }
                  ]
              }
        (Just token, _) -> do
          a1 <- async $ getChannels token $ tail args
          a2 <- async $ getMembers token $ tail args
          channels <- wait a1
          members <- wait a2
          let items =
                if null channels && null members
                  then [ Item
                           { uid = ""
                           , title = "NO MATCH."
                           , subtitle = "Please change keyword."
                           , arg = "''"
                           , icon = Nothing
                           }
                       ]
                  else members ++ channels -- Note: There are so many channels, so I'll prioritize members.
          usedArgs <- loadUsedArgs
          let sortedItems = sortUsedArgsFirst items usedArgs
          putStrLn $
            convertString $
            encode $
            SearchResult
              { skipknowledge = True
              , variables = Variables {oldResults = "[]", oldArgv = "[]"}
              , items = sortedItems
              }
    _ -> exitFailure
