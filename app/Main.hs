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
dispatchCommand (Init path) = initCommand path
dispatchCommand (CatFile _ object) = catFileCommand object