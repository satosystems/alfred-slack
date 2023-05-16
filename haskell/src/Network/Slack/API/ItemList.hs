{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.API.ItemList
    -- * API request
  ( getChannels
  , getMembers
    -- * Cache control
  , cacheFile
  , clearChannelsCache
  , clearMembersCache
  ) where

import Control.Concurrent.Async
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.CaseInsensitive as CI
import Data.Char
import Data.List (isInfixOf)
import Data.Maybe
import Data.String.Conversions
import qualified Data.Text as T
import Data.Text.Normalize
import Network.HTTP.Simple
import System.Directory

import Network.Slack.API.ItemListResponse
import Types

apiPathChannels :: Path
apiPathChannels = "api/conversations.list"

apiPathMembers :: Path
apiPathMembers = "api/users.list"

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
foldHeaders acc (k, v) = (CI.mk k, convertString v) : acc

request :: Token -> Path -> IO ([Channel], [Member])
request token path = do
  let cache = cacheFile path
  exist <- doesFileExist cache
  if exist
    then do
      contents <- readFile cache
      return $ read contents
    else do
      results <- go ([], []) ""
      writeCacheJSON cache $ show results
      return results
  where
    go :: ([Channel], [Member]) -> Cursor -> IO ([Channel], [Member])
    go acc@(channels, members) cursor = do
      let req =
            makeRequest
              "GET"
              [("Authorization", "Bearer " ++ token)]
              (convertString path) $
            [("limit", "99999"), ("exclude_archived", "true")] ++
            [("cursor", cursor) | (not . null) cursor] ++
            [ ("types", "public_channel,private_channel,mpim")
            | path == apiPathChannels
            ]
      res <- httpJSON req
      let ListResponse ok mChannels mMembers mMetadata = getResponseBody res
      if ok
        then let nc =
                   responseMetadataNextCursor $
                   fromMaybe (ResponseMetadata "") mMetadata
                 channels' = fromMaybe [] mChannels
                 members' = fromMaybe [] mMembers
                 acc' = (channels ++ channels', members ++ members')
              in if null nc
                   then return acc'
                   else go acc' nc
        else return acc

infixOfIgnoreCase :: String -> String -> Bool
infixOfIgnoreCase needle haystack =
  let needle' =
        map toLower $ (convertString . normalize NFC . convertString) needle
      haystack' = map toLower haystack
   in needle' `isInfixOf` haystack'

foldToItemFromChannel :: Maybe String -> [Item] -> Channel -> [Item]
foldToItemFromChannel _ acc (Channel _ _ True _ _) = acc
foldToItemFromChannel Nothing acc (Channel id' name _ teamId (Purpose value)) =
  Item
    id'
    name
    value
    ("'slack://channel?team=" ++ teamId ++ "&id=" ++ id' ++ "'")
    Nothing :
  acc
foldToItemFromChannel (Just keyword) acc (Channel id' name _ teamId (Purpose value))
  | keyword `infixOfIgnoreCase` name =
    Item
      id'
      name
      value
      ("'slack://channel?team=" ++ teamId ++ "&id=" ++ id' ++ "'")
      Nothing :
    acc
  | otherwise = acc

-- |
-- Get path from URL.
--
-- >>> imagePath "https://a.slack-edge.com/80588/img/slackbot_48.png"
-- >>> "80588/img/slackbot_48.png"
imagePath :: URL -> FilePath
imagePath url =
  let split = T.splitOn "/" $ convertString url
      split' = drop 3 split
   in convertString $ T.intercalate "/" split'

foldToItemFromMember :: Maybe String -> [Item] -> Member -> [Item]
foldToItemFromMember _ acc (Member _ _ _ True _ _) = acc
foldToItemFromMember _ acc (Member _ _ _ _ _ True) = acc
foldToItemFromMember Nothing acc (Member id' teamId name _ (Profile realName displayName image) _) =
  Item
    id'
    displayName
    (realName ++ " (" ++ name ++ ")")
    ("'slack://user?team=" ++ teamId ++ "&id=" ++ id' ++ "'")
    (Just $ ImagePath $ "./.cache/" ++ imagePath image) :
  acc
foldToItemFromMember (Just keyword) acc (Member id' teamId name _ (Profile realName displayName image) _)
  | any (keyword `infixOfIgnoreCase`) [name, realName, displayName] =
    Item
      id'
      displayName
      (realName ++ " (" ++ name ++ ")")
      ("'slack://user?team=" ++ teamId ++ "&id=" ++ id' ++ "'")
      (Just $ ImagePath $ "./.cache/" ++ imagePath image) :
    acc
  | otherwise = acc

getChannels :: Token -> Maybe String -> IO [Item]
getChannels token mKeyword = do
  (channels, _) <- request token apiPathChannels
  return $ foldl (foldToItemFromChannel mKeyword) [] channels

getMembers :: Token -> Maybe String -> IO [Item]
getMembers token mKeyword = do
  (_, members) <- request token apiPathMembers
  isNothing mKeyword `when` downloadImage members
  return $ foldl (foldToItemFromMember mKeyword) [] members

downloadImage :: [Member] -> IO ()
downloadImage members = do
  mapConcurrently_ go members
  where
    go member = do
      let image = (profileImage_48 . memberProfile) member
      let filePath = (cacheFile . imagePath) image
      exist <- doesFileExist filePath
      exist `unless` do
        let req = parseRequest_ $ "GET " ++ image
        res <- httpLBS req
        let body = getResponseBody res
        writeCacheImage filePath body

parentDirectory :: FilePath -> FilePath
parentDirectory filePath =
  convertString $
  T.intercalate "/" $ init $ T.splitOn "/" $ convertString filePath

writeCacheJSON :: FilePath -> String -> IO ()
writeCacheJSON filePath contents = do
  let dir = parentDirectory filePath
  createDirectoryIfMissing True dir
  writeFile filePath contents

writeCacheImage :: FilePath -> LB.ByteString -> IO ()
writeCacheImage filePath contents = do
  let dir = parentDirectory filePath
  createDirectoryIfMissing True dir
  LB.writeFile filePath contents

cacheFile :: Path -> FilePath
cacheFile path = ".cache/" ++ path

removeFileIfExist :: FilePath -> IO ()
removeFileIfExist filePath = do
  exist <- doesFileExist filePath
  exist `when` removeFile filePath

clearChannelsCache :: IO ()
clearChannelsCache = (removeFileIfExist . cacheFile) apiPathChannels

clearMembersCache :: IO ()
clearMembersCache = (removeFileIfExist . cacheFile) apiPathMembers
