module Main where

import Options.Applicative
import Wyag

main :: IO ()
main = greet =<< execParser opts
  where
    opts =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Welcome to Write Yourself a Git"
            <> header "hello - a test for optparse-applicative"
        )

-- opts :: P.ParserInfo Command
-- opts = P.info command

greet :: Command -> IO ()
greet = print