module Main where

import System.Environment
import qualified Data.Set as Set
import Crawler

main = do
  [src, depth] <- getArgs
  walkPage src (read depth :: Int) Set.empty []
