module Main
  ( main
  ) where

import Data.String.Conversions (cs)
import System.Environment (getArgs)

import Alfred (main')

main :: IO ()
main = getArgs >>= main' . map cs
