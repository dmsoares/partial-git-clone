module Parser where

import Commands
import Options.Applicative
import Prelude hiding (init)

commandParser :: Parser Command
commandParser = subparser $ mconcat [command "init" (info init (progDesc "Initialize a new, empty repository"))]

init :: Parser Command
init = Init <$> strArgument (metavar "path" <> value "." <> help "Where to create the repository")
