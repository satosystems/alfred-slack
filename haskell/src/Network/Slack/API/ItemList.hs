{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.API.ItemList
  ( request
  ) where

import qualified Data.ByteString as B
import Data.CaseInsensitive
import Data.Maybe
import Data.String.Conversions
import Foreign.C.Types
import Network.HTTP.Simple
import System.Directory
import System.Posix.Time

import Network.Slack.API.ItemListResponse
import Network.Slack.Types

makeRequest ::
     B.ByteString
  -> [(B.ByteString, String)]
  -> B.ByteString
  -> [(B.ByteString, String)]
  -> Request
makeRequest method headers path query =
  let headers' = foldl foldHeaders [] headers
      query' = foldl foldQueryString [] query
      req0 = parseRequest_ "https://api.slack.com/"
      req1 = setRequestMethod method req0
      req2 = setRequestHeaders headers' req1
      req3 = setRequestPath path req2
      req4 = setRequestQueryString query' req3
   in req4

foldQueryString :: Query -> (B.ByteString, String) -> Query
foldQueryString acc (k, v) = (k, Just $ convertString v) : acc

foldHeaders :: RequestHeaders -> (B.ByteString, String) -> RequestHeaders
foldHeaders acc (k, v) = (mk k, convertString v) : acc

request :: Token -> Path -> IO ([Channel], [Member])
request token path = do
  let cacheFile = ".cache." ++ drop 4 path
  exist <- doesFileExist cacheFile
  (cacheChannels, cacheMembers, cacheTs) <-
    if exist
      then do
        contents <- readFile cacheFile
        return $ read contents
      else return ([], [], 0)
  CTime currentTime <- epochTime
  if currentTime < cacheTs + 86400
    then return (cacheChannels, cacheMembers)
    else do
      (channels, members) <- go ([], []) ""
      writeFile cacheFile $ show (channels, members, currentTime)
      return (channels, members)
  where
    go :: ([Channel], [Member]) -> Cursor -> IO ([Channel], [Member])
    go acc@(channels, members) cursor = do
      let req =
            makeRequest
              "GET"
              [("Authorization", "Bearer " ++ token)]
              (convertString path)
              ([("cursor", cursor) | (not . null) cursor])
      res <- httpJSON req
      let ListResponse ok mChannels mMembers metadata = getResponseBody res
      if ok
        then let nc = responseMetadataNextCursor metadata
                 channels' = fromMaybe [] mChannels
                 members' = fromMaybe [] mMembers
                 acc' = (channels ++ channels', members ++ members')
              in if null nc
                   then return acc'
                   else go acc' nc
        else return acc
