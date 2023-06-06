{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.API.ItemList
    -- * API request
  ( getChannels
  , getMembers
  , searchMessages
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
import Data.Maybe
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Normalize
import Network.HTTP.Simple
import System.Directory

import Network.Slack.API.ItemListResponse
import Types

apiPathChannels :: Path
apiPathChannels = "api/conversations.list"

apiPathMembers :: Path
apiPathMembers = "api/users.list"

apiPathMessages :: Path
apiPathMessages = "api/search.messages"

makeRequest ::
     B.ByteString
  -> [(B.ByteString, T.Text)]
  -> B.ByteString
  -> [(B.ByteString, T.Text)]
  -> Request
makeRequest method headers path' query =
  let headers' = foldl foldHeaders [] headers
      query' = foldl foldQueryString [] query
      req0 = parseRequest_ "https://api.slack.com/"
      req1 = setRequestMethod method req0
      req2 = setRequestHeaders headers' req1
      req3 = setRequestPath path' req2
      req4 = setRequestQueryString query' req3
   in req4

foldQueryString :: Query -> (B.ByteString, T.Text) -> Query
foldQueryString acc (k, v) = (k, Just $ cs v) : acc

foldHeaders :: RequestHeaders -> (B.ByteString, T.Text) -> RequestHeaders
foldHeaders acc (k, v) = (CI.mk k, cs v) : acc

getChannelsOrMembers :: Token -> Path -> IO ([Channel], [Member])
getChannelsOrMembers token path' = do
  let cache = cacheFile path'
  exist <- doesFileExist cache
  if exist
    then do
      contents <- readFile cache
      return $ read contents
    else do
      results <- go ([], []) ""
      writeCacheJSON cache $ (cs . show) results
      return results
  where
    go :: ([Channel], [Member]) -> Cursor -> IO ([Channel], [Member])
    go acc@(channels, members) cursor = do
      let req =
            makeRequest
              "GET"
              [("Authorization", "Bearer " +++ token)]
              (cs path') $
            [("limit", "99999"), ("exclude_archived", "true")] ++
            [("cursor", cursor) | (not . T.null) cursor] ++
            [ ("types", "public_channel,private_channel,mpim")
            | path' == apiPathChannels
            ]
      res <- httpJSON req
      let ListResponse ok mChannels mMembers _ mMetadata = getResponseBody res
      if ok
        then let nc =
                   responseMetadataNextCursor $
                   fromMaybe (ResponseMetadata "") mMetadata
                 channels' = fromMaybe [] mChannels
                 members' = fromMaybe [] mMembers
                 acc' = (channels ++ channels', members ++ members')
              in if T.null nc
                   then return acc'
                   else go acc' nc
        else return acc

infixOfIgnoreCase :: T.Text -> T.Text -> Bool
infixOfIgnoreCase needle haystack =
  let needle' = T.toLower $ normalize NFC needle
      haystack' = T.toLower haystack
   in needle' `T.isInfixOf` haystack'

formatPrettyMpdmIfNeeded :: T.Text -> T.Text
formatPrettyMpdmIfNeeded name
  | "mpdm-" `T.isPrefixOf` name && "-1" `T.isSuffixOf` name =
    T.intercalate ", " $ T.splitOn "--" $ (T.init . T.init . T.drop 5) name
  | otherwise = name

foldToItemFromChannel :: [T.Text] -> [Item] -> Channel -> [Item]
foldToItemFromChannel _ acc (Channel _ _ True _ _) = acc
foldToItemFromChannel [] acc (Channel id' name _ teamId (Purpose value)) =
  Item
    id'
    (formatPrettyMpdmIfNeeded name)
    value
    ("'slack://channel?team=" +++ teamId +++ "&id=" +++ id' +++ "'")
    Nothing :
  acc
foldToItemFromChannel keywords acc (Channel id' name _ teamId (Purpose value))
  | all (`infixOfIgnoreCase` name) keywords =
    Item
      id'
      (formatPrettyMpdmIfNeeded name)
      value
      ("'slack://channel?team=" +++ teamId +++ "&id=" +++ id' +++ "'")
      Nothing :
    acc
  | otherwise = acc

-- |
-- Get path from URL.
--
-- >>> imagePath "https://a.slack-edge.com/80588/img/slackbot_48.png"
-- "80588/img/slackbot_48.png"
imagePath :: URL -> FilePath
imagePath url =
  let split = T.splitOn "/" $ cs url
      split' = drop 3 split
   in cs $ T.intercalate "/" split'

foldToItemFromMember :: [T.Text] -> [Item] -> Member -> [Item]
foldToItemFromMember _ acc (Member _ _ _ True _ _) = acc
foldToItemFromMember _ acc (Member _ _ _ _ _ True) = acc
foldToItemFromMember [] acc (Member id' teamId name _ (Profile realName displayName image) _) =
  Item
    id'
    displayName
    (realName +++ " (" +++ name +++ ")")
    ("'slack://user?team=" +++ teamId +++ "&id=" +++ id' +++ "'")
    (Just $ ImagePath $ "./.cache/" ++ imagePath image) :
  acc
foldToItemFromMember keywords acc (Member id' teamId name _ (Profile realName displayName image) _)
  | all
     (\keyword ->
        any (keyword `infixOfIgnoreCase`) [name, realName, displayName])
     keywords =
    Item
      id'
      displayName
      (realName +++ " (" +++ name +++ ")")
      ("'slack://user?team=" +++ teamId +++ "&id=" +++ id' +++ "'")
      (Just $ ImagePath $ "./.cache/" ++ imagePath image) :
    acc
  | otherwise = acc

getChannels :: Token -> [T.Text] -> IO [Item]
getChannels token keywords = do
  (channels, _) <- getChannelsOrMembers token apiPathChannels
  return $ foldl (foldToItemFromChannel keywords) [] channels

getMembers :: Token -> [T.Text] -> IO [Item]
getMembers token keywords = do
  (_, members) <- getChannelsOrMembers token apiPathMembers
  null keywords `when` downloadImage members
  return $ foldl (foldToItemFromMember keywords) [] members

searchMessages :: Token -> T.Text -> IO [Item]
searchMessages token query = do
  let req =
        makeRequest
          "GET"
          [("Authorization", "Bearer " +++ token)]
          (cs apiPathMessages)
          [("count", "999999999999999999"), ("query", query)]
  res <- httpJSON req
  let ListResponse ok _ _ mMessages _ = getResponseBody res
  if ok
    then case mMessages of
           Nothing -> return []
           Just (Messages matches) -> return $ map toItem matches
    else return []
  where
    toItem :: Match -> Item
    toItem (Match iid (MatchChannel isChannel isGroup isMpim name) username text permalink) =
      Item iid text subtitle' permalink Nothing
      where
        subtitle' :: T.Text
        subtitle' =
          case (isChannel || isGroup, isMpim) of
            (_, True) -> formatPrettyMpdmIfNeeded name
            (True, _) -> name
            _ -> username

downloadImage :: [Member] -> IO ()
downloadImage members = do
  mapConcurrently_ go members
  where
    go member = do
      let image = (profileImage_48 . memberProfile) member
      let filePath = (cacheFile . imagePath) image
      exist <- doesFileExist filePath
      exist `unless` do
        let req = parseRequest_ $ "GET " ++ cs image
        res <- httpLBS req
        let body = getResponseBody res
        writeCacheImage filePath body

parentDirectory :: FilePath -> FilePath
parentDirectory filePath =
  cs $ T.intercalate "/" $ init $ T.splitOn "/" $ cs filePath

writeCacheJSON :: FilePath -> T.Text -> IO ()
writeCacheJSON filePath contents = do
  let dir = parentDirectory filePath
  createDirectoryIfMissing True dir
  TIO.writeFile filePath contents

writeCacheImage :: FilePath -> LB.ByteString -> IO ()
writeCacheImage filePath contents = do
  let dir = parentDirectory filePath
  createDirectoryIfMissing True dir
  LB.writeFile filePath contents

cacheFile :: Path -> FilePath
cacheFile path' = ".cache/" ++ path'

removeFileIfExist :: FilePath -> IO ()
removeFileIfExist filePath = do
  exist <- doesFileExist filePath
  exist `when` removeFile filePath

clearChannelsCache :: IO ()
clearChannelsCache = (removeFileIfExist . cacheFile) apiPathChannels

clearMembersCache :: IO ()
clearMembersCache = (removeFileIfExist . cacheFile) apiPathMembers
