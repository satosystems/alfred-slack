{-# LANGUAGE OverloadedStrings #-}

module Alfred
  ( main'
  ) where

import Control.Concurrent.Async
import Data.Aeson
import Data.String.Conversions
import Data.Time.Clock.System

import Control.Monad
import Network.Slack.API.ItemList
import Types

main' :: Token -> String -> IO ()
main' token keyword = do
  case keyword of
    "--update" -> do
      t1 <- getSystemTime
      a1 <- async clearChannelsCache
      a2 <- async clearMembersCache
      mapM_ wait [a1, a2]
      a3 <- async $ void $ getChannels token Nothing
      a4 <- async $ void $ getMembers token Nothing
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
    _ -> do
      a1 <- async $ getChannels token $ Just keyword
      a2 <- async $ getMembers token $ Just keyword
      channels <- wait a1
      members <- wait a2
      let items =
            if null channels && null members
              then [ Item
                       { uid = ""
                       , title = "NO MATCH."
                       , subtitle = "Please change keyword."
                       , arg = ""
                       , icon = Nothing
                       }
                   ]
              else members ++ channels -- Note: There are so many channels, so I'll prioritize members.
      putStrLn $
        convertString $
        encode $
        SearchResult
          { skipknowledge = True
          , variables = Variables {oldResults = "[]", oldArgv = "[]"}
          , items = items
          }
