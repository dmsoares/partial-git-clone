module Main where

import Options.Applicative
import Wyag.Cli
import Wyag.Core

main :: IO ()
main = dispatchCommand =<< execParser opts
  where
    opts =
      info
        (commandParser <**> helper)
        (fullDesc <> progDesc "Welcome to Write Yourself a Git")