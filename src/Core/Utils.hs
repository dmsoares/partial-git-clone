module Core.Utils where

import System.Directory

isDirectoryEmpty :: FilePath -> IO Bool
isDirectoryEmpty fp = null <$> listDirectory fp