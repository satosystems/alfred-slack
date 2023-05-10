{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Alfred
  ( main'
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.List (isInfixOf)
import Data.String.Conversions
import System.Exit

import Network.Slack.API.ItemList
import Network.Slack.API.ItemListResponse
import Network.Slack.Types

data Item =
  Item
    { uid :: String
    , title :: String
    , subtitle :: String
    , arg :: String
    -- , autocomplete :: String
    -- , icon :: Icon
    }
  deriving (Show)

$(deriveJSON defaultOptions ''Item)

foldToItemFromChannel :: String -> [Item] -> Channel -> [Item]
foldToItemFromChannel _ acc (Channel _ _ True _ _) = acc
foldToItemFromChannel keyword acc (Channel id' name _ teamId (Purpose value))
  | keyword `isInfixOf` name = Item id' name value (teamId ++ " " ++ id') : acc
  | otherwise = acc

foldToItemFromMember :: String -> [Item] -> Member -> [Item]
foldToItemFromMember _ acc (Member _ _ _ True _ _) = acc
foldToItemFromMember _ acc (Member _ _ _ _ _ True) = acc
foldToItemFromMember keyword acc (Member id' teamId name _ (Profile realName displayName _) _)
  | any (keyword `isInfixOf`) [name, realName, displayName] =
    Item
      id'
      displayName
      (realName ++ " (" ++ name ++ ")")
      (teamId ++ " " ++ id') :
    acc
  | otherwise = acc

getChannels :: Token -> String -> IO [Item]
getChannels token keyword = do
  (channels, _) <- request token "api/conversations.list"
  return $ foldl (foldToItemFromChannel keyword) [] channels

getMembers :: Token -> String -> IO [Item]
getMembers token keyword = do
  (_, members) <- request token "api/users.list"
  return $ foldl (foldToItemFromMember keyword) [] members

main' :: Token -> String -> IO ()
main' token keyword = do
  case (head keyword, length keyword == 1) of
    ('#', True) -> exitSuccess
    ('@', True) -> exitSuccess
    ('#', _) -> do
      channels <- getChannels token $ tail keyword
      putStrLn $ convertString $ encode channels
    ('@', _) -> do
      members <- getMembers token $ tail keyword
      putStrLn $ convertString $ encode members
    _ -> do
      channels <- getChannels token keyword
      members <- getMembers token keyword
      putStrLn $ convertString $ encode $ channels ++ members
