module Main where

import Options.Applicative
import Wyag

main :: IO ()
main = greet =<< execParser opts
  where
    opts =
      info
        (commandParser <**> helper)
        (fullDesc <> progDesc "Welcome to Write Yourself a Git")

-- opts :: P.ParserInfo Command
-- opts = P.info command

greet :: Command -> IO ()
greet (Init path) = initRepository path