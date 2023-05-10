{-# LANGUAGE OverloadedStrings #-}

module Alfred
  ( main'
  ) where

import Control.Concurrent.Async
import Data.Aeson
import Data.String.Conversions

import Control.Monad
import Network.Slack.API.ItemList
import Types

main' :: Token -> String -> IO ()
main' token keyword = do
  case keyword of
    "--update" -> do
      a1 <- async clearChannelsCache
      a2 <- async clearMembersCache
      a3 <- async $ void $ getChannels token Nothing
      a4 <- async $ void $ getMembers token Nothing
      mapM_ wait [a1, a2, a3, a4]
    _ -> do
      a1 <- async $ getChannels token $ Just keyword
      a2 <- async $ getMembers token $ Just keyword
      channels <- wait a1
      members <- wait a2
      putStrLn $ convertString $ encode $ channels ++ members
