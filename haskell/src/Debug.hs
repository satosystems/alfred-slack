module Debug
  ( debug
  , debugWriteCacheJSON
  ) where

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as LB

debug :: Show a => a -> IO ()
debug = appendFile "./alfred-slack.log" . (++ "\n") . show

debugWriteCacheJSON :: (ToJSON a) => a -> FilePath -> IO ()
debugWriteCacheJSON value path = LB.writeFile path (encode value)
