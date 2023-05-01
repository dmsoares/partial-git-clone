module Utils where

import System.Directory

isDirectoryEmpty :: FilePath -> IO Bool
isDirectoryEmpty fp = do
  contents <- listDirectory fp
  return $ null contents