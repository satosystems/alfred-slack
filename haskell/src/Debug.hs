module Debug
  ( debug
  ) where

debug :: Show a => a -> IO ()
debug = appendFile "./alfred-slack.log" . (++ "\n") . show
