module Main
  ( main
  ) where

import Control.Monad
import System.Environment
import System.Exit

import Alfred (main')

main :: IO ()
main = do
  args <- getArgs
  (length args < 2) `when` exitFailure
  let token = head args
  let keyword = args !! 1
  main' token keyword
