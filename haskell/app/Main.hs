module Main
  ( main
  ) where

import System.Environment

import Alfred (main')
import Data.String.Conversions

main :: IO ()
main = getArgs >>= main' . map cs
