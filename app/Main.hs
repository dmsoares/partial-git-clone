module Main where

import Options.Applicative
import Wyag

main :: IO ()
main = dispatchCommand =<< execParser opts
  where
    opts =
      info
        (commandParser <**> helper)
        (fullDesc <> progDesc "Welcome to Write Yourself a Git")

dispatchCommand :: Command -> IO ()
dispatchCommand (Init path) = initAction path
dispatchCommand (CatFile _ object) = catFileAction object
dispatchCommand (HashObject typ w path) = hashObjectAction typ w path