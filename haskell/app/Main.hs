module Main
  ( main
  ) where

import System.Environment

import Alfred (main')

main :: IO ()
main = getArgs >>= main'
