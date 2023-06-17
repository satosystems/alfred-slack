module Main
  ( main
  ) where

import Data.String.Conversions
import System.Environment

import Alfred (main')

main :: IO ()
main = getArgs >>= main' . map cs
