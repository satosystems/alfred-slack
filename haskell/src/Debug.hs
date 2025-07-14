module Debug
  ( debug
  , debugWriteCacheJSON
  ) where

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as LB
import System.IO (hPutStrLn, stderr)

debug :: Show a => a -> IO ()
debug x = hPutStrLn stderr $ "\ESC[35m" ++ show x ++ "\ESC[0m"

debugWriteCacheJSON :: (ToJSON a) => a -> FilePath -> IO ()
debugWriteCacheJSON value path = LB.writeFile path (encode value)
