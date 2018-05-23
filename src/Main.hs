module Main where

import qualified Cell
import qualified System.Environment as Env

main = do
  [boardFileName] <- Env.getArgs
  boardFile <- readFile boardFileName
  print boardFile
  -- print (Cell.fromString boardFile)
