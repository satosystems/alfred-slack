{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Debug
  ( debug
  , debugWriteCacheJSON
  ) where

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)

class DebugOutput a where
  toString :: a -> String

instance DebugOutput String where
  toString = id

instance DebugOutput Int where
  toString = init . tail . show

instance DebugOutput Bool where
  toString = init . tail . show

instance DebugOutput T.Text where
  toString = T.unpack

debug :: DebugOutput a => a -> IO ()
debug x = hPutStrLn stderr $ "\ESC[35m" ++ toString x ++ "\ESC[0m"

debugWriteCacheJSON :: (ToJSON a) => a -> FilePath -> IO ()
debugWriteCacheJSON value path = LB.writeFile path (encode value)
