{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# HLINT ignore "Avoid lambda" #-}
module Alfred
  ( main'
  ) where

import Control.Concurrent.Async (async, wait)
import Control.Monad (void, when)
import Data.Aeson (encode)
import Data.List (find, sortOn)
import Data.List.Utils (uniq)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Data.Time.Clock.System
  ( SystemTime(systemNanoseconds, systemSeconds)
  , getSystemTime
  )
import Network.URI (escapeURIString, isUnreserved, unEscapeString)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import qualified System.IO.Strict as SIO
import System.Process (system)

import Debug (debug)
import Slack
  ( cacheFile
  , clearChannelsCache
  , clearMembersCache
  , getChannels
  , getMembers
  , searchMessages
  )
import Types
  ( ImagePath(ImagePath)
  , Item(Item, arg, icon, kind, subtitle, title, uid)
  , Kind(KindChannel, KindCommandResult, KindGroupMessage, KindMember,
     KindMessage, KindNoMatch, KindOops)
  , SearchResult(SearchResult, items, skipknowledge, variables)
  , Token
  , Variables(Variables, oldArgv, oldResults)
  , (+++)
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
  if "slack://" `T.isPrefixOf` url
    then do
      _ <- system $ "open '" ++ cs url ++ "'"
      return ()
    else do
      let command =
            unEscapeString $ cs $ T.drop (T.length "alfred-slack://") url
      debug command
      _ <- system command
      return ()

sortItemsByTitle :: [Item] -> [Item]
sortItemsByTitle = sortOn title

main' :: [T.Text] -> IO ()
main' args = do
  debug args
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
                , kind = KindOops
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
                      , kind = KindCommandResult
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
          let specialSearchTuples ::
                   [( T.Text
                    , T.Text -> ( T.Text
                                , T.Text
                                , Maybe (Token -> [T.Text] -> IO [Item])
                                , Maybe (Item -> String)))]
              specialSearchTuples =
                [ ("in:#", ("in:#", , Just getChannels, Just $ cs . title))
                , ( "from:@"
                  , ( "from:@"
                    ,
                    , Just getMembers
                    , Just $ (cs . last . init . T.splitOn ")") .
                      (last . T.splitOn "(") .
                      subtitle))
                ]
          let specialSearchList ::
                   [( T.Text
                    , T.Text
                    , Maybe (Token -> [T.Text] -> IO [Item])
                    , Maybe (Item -> String))]
              specialSearchList =
                foldl
                  (\acc (f, s) ->
                     let v@(_, _, t, _) =
                           maybe
                             ("", "", Nothing, Nothing)
                             s
                             (find (f `T.isPrefixOf`) keywords)
                      in if isNothing t
                           then acc
                           else v : acc)
                  []
                  specialSearchTuples
          -- let (prefix, specialKeyword, getter, fn) =
          --       case ( find (\keyword -> "in:#" `T.isPrefixOf` keyword) keywords
          --            , find ("from:@" `T.isPrefixOf`) keywords) of
          --         (Just v, _) ->
          --           ("in:#", v, Just getChannels, Just $ cs . title)
          --         (_, Just v) ->
          --           ( "from:@"
          --           , v
          --           , Just getMembers
          --           , Just $
          --             (cs . last . init . T.splitOn ")") .
          --             (last . T.splitOn "(") . subtitle)
          --         _ -> ("", "", Nothing, Nothing)
          -- 特別な検索結果がここに格納される。
          -- head には in:# の検索結果が、last には from:@ の検索結果が格納される。
          specialSearchItems <-
            mapM
              (\(prefix, specialKeyword, getter, fn) -> do
                 if isJust fn
                   then do
                     let specialKeywords =
                           T.drop (T.length prefix) specialKeyword
                     let partOfItemName =
                           map (T.replace " " "") $
                           T.splitOn "," specialKeywords
                     a3 <- async $ fromJust getter token partOfItemName
                     specialSearchItems <- wait a3
                     let searchText = T.intercalate " " $ "" : init keywords
                     return $
                       map
                         (\item ->
                            item
                              { arg =
                                  if title item == specialKeywords
                                    then arg item
                                    else Just $
                                         cs $
                                         "alfred-slack://" ++
                                         escapeURIString
                                           isUnreserved
                                           ("/usr/bin/osascript -e 'tell application \"Alfred 5\" to search \"ss" ++
                                            cs searchText ++
                                            " \\\"" ++
                                            cs prefix ++
                                            fromJust fn item ++ "\\\"\"'")
                              , icon =
                                  Just $
                                  ImagePath $
                                  if title item == specialKeywords
                                    then "./slack.png"
                                    else "./alfred.png"
                              })
                         (filter
                            (\item -> title item `notElem` partOfItemName)
                            specialSearchItems)
                   else return [])
              specialSearchList
          debug ("specialSearchItems" :: String, specialSearchItems)
          debug ("----------" :: String)
          let complementItems =
                foldl
                  (\acc item ->
                     let isPrefixAlfredSlack =
                           "alfred-slack://" `T.isPrefixOf`
                           fromMaybe "" (arg item)
                         isGroupMessaging =
                           "Group messaging with:" `T.isPrefixOf` subtitle item
                         isNotGroupMessaging = not isGroupMessaging
                         (prefix0, fn0) = head specialSearchTuples
                         (prefix1, fn1) = last specialSearchTuples
                         (_, _, _, Just fn0') = fn0 ""
                         (_, _, _, Just fn1') = fn1 ""
                         lastKeyword = last keywords
                         isPrefixIn = "in:#" `T.isPrefixOf` lastKeyword
                         isPrefixFrom = "from:@" `T.isPrefixOf` lastKeyword
                         isEnoughtLength0 =
                           T.length lastKeyword > T.length prefix0
                         isEnoughtLength1 =
                           T.length lastKeyword > T.length prefix1
                         title' = (cs . fn0') item
                         subtitle' = (cs . fn1') item
                         droppedKeyword0 = T.drop (T.length prefix0) lastKeyword
                         droppedKeyword1 = T.drop (T.length prefix1) lastKeyword
                         isPartOfChannelName =
                           droppedKeyword1 `T.isPrefixOf` title'
                         isPartOfUserName =
                           droppedKeyword1 `T.isPrefixOf` subtitle'
                         isEqualChannelName = droppedKeyword0 == title'
                         isEqualUserName = droppedKeyword1 == subtitle'
                      in case ( isPrefixAlfredSlack &&
                                isGroupMessaging &&
                                isPrefixIn &&
                                isEnoughtLength0 && isPartOfChannelName
                              , isPrefixAlfredSlack &&
                                isNotGroupMessaging &&
                                isPrefixFrom &&
                                isEnoughtLength1 && isPartOfUserName) of
                           (True, _) -> (isEqualChannelName, item) : acc
                           (_, True) -> (isEqualUserName, item) : acc
                           _ -> (False, item) : acc)
                  [] $
                concat specialSearchItems
          debug ("complementItems" :: String, complementItems)
          debug ("----------" :: String)
          -- let candidateItems' =
          --       case lookup True complementItems of
          --         Nothing -> map snd complementItems
          --         _ -> concat specialSearchItems
          -- let matchedChannels =
          --       filter
          --         (\item -> title item `elem` any (\keyword -> if "in:#" `T.isPrefixOf` keyword && ) keywords)
          --         candidateItems'
          let mTargetChannel =
                let filteredKeywords = filter ("in:#" `T.isPrefixOf`) keywords
                    matchedChannels =
                      filter
                        (\item ->
                           not (null filteredKeywords) &&
                           (T.drop (T.length "in:#") (head filteredKeywords) ==
                            title item)) $
                      map snd complementItems
                 in if (not . null) matchedChannels
                      then (Just . uid . head) matchedChannels
                      else Nothing
          debug ("mTargetChannel" :: String, mTargetChannel)
          let excludedSpecialKeywords =
                case mTargetChannel of
                  Nothing ->
                    filter
                      (\keyword -> not $ "from:@" `T.isPrefixOf` keyword)
                      keywords
                  Just _ ->
                    filter
                      (\keyword -> not $ "in:#" `T.isPrefixOf` keyword)
                      keywords
          debug ("excludedSpecialKeywords" :: String, excludedSpecialKeywords)
          debug
            ( "--- condition ---" :: String
            , ( any (`T.isPrefixOf` last keywords) ["in:#", "from:@"]
              , lookup True complementItems
              , (mTargetChannel, (not . null) excludedSpecialKeywords)
              , (not . null) channels || (not . null) members))
          items' <-
            case ( any (`T.isPrefixOf` last keywords) ["in:#", "from:@"]
                 , lookup True complementItems
                 , (mTargetChannel, (not . null) excludedSpecialKeywords)
                 , (not . null) channels || (not . null) members) of
              (True, Nothing, _, _)
               -- 特殊検索でかつ特殊検索が完了状態ではない場合は、特殊検索の補完を行う
               -> do
                debug ("---- 01" :: String)
                return $
                  filter
                    (\item ->
                       if "from:@" `T.isPrefixOf` last keywords
                         then kind item == KindMember
                         else kind item `elem` [KindChannel, KindGroupMessage]) $
                  map snd complementItems
              (True, Just _, (_, False), _)
               -- 特殊検索でかつ特殊検索が完了状態の場合、かつ検索キーワードが未入力の場合は検索結果なしにする
               -> do
                debug ("---- 02" :: String)
                return
                  [ Item
                      { uid = ""
                      , title = "NO MATCH."
                      , subtitle = "Please change keyword."
                      , kind = KindNoMatch
                      , arg = Nothing
                      , icon = Nothing
                      }
                  ]
              (_, _, (Just targetChannel, True), _)
               -- 検索キーワードが入力されていれば検索を行い、フィルタする
               -> do
                debug ("---- 03" :: String)
                items' <-
                  searchMessages token $
                  T.intercalate " " excludedSpecialKeywords
                return $
                  filter
                    (\item ->
                       "id=" +++
                       targetChannel `T.isInfixOf` fromMaybe "" (arg item) &&
                       KindMessage == kind item)
                    items'
              (_, _, (Nothing, True), _)
               -- 検索キーワードが入力されていれば検索を行う
               -> do
                debug ("---- 04" :: String)
                searchMessages token $ T.intercalate " " excludedSpecialKeywords
              (_, _, _, True)
                -- メンバーかチャンネルが取得できていれば、それらを結合して返す（チャンネルは数が多いのでメンバーを優先）
               -> do
                debug ("---- 05" :: String)
                return $ sortItemsByTitle members ++ sortItemsByTitle channels
              _
                -- それ以外の場合は検索結果を返す
               -> do
                debug ("---- 06" :: String)
                searchMessages token $ T.intercalate " " excludedSpecialKeywords
          debug ("@@@@@@ mTargetChannel" :: String, mTargetChannel)
          debug ("$$$$$$ items'" :: String, items')
          -- チャンネルが特定されている場合は、検索結果をそのチャンネルにのみ絞り込む
          -- let items'' =
          --       filter
          --         (\item ->
          --            case mTargetChannel of
          --              Nothing -> True
          --              Just targetChannel ->
          --                "id=" +++
          --                targetChannel `T.isInfixOf` fromMaybe "" (arg item) &&
          --                KindMessage == kind item)
          --         items'
          debug ("###### items'" :: String, mTargetChannel, items')
          let items'' =
                if null items'
                  then [ Item
                           { uid = ""
                           , title = "NO MATCH."
                           , subtitle = "Please change keyword."
                           , kind = KindNoMatch
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
