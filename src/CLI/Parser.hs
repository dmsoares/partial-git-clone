module CLI.Parser where

import Core.Commands
import Core.Object hiding (Parser, objectTypeP)
import Data.Void
import Options.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (init)

commandParser :: Parser Command
commandParser =
  subparser $
    mconcat
      [ command "init" (info init (progDesc "Initialize a new, empty repository")),
        command "cat-file" (info catFile (progDesc "Provide content of repository objects"))
      ]

init :: Parser Command
init = Init <$> strArgument (metavar "path" <> value "." <> help "Where to create the repository")

catFile :: Parser Command
catFile =
  CatFile
    <$> argument (maybeReader objectTypeP) (metavar "type" <> help "Specify the type")
    <*> strArgument (metavar "object" <> help "The object to display")

objectTypeP :: String -> Maybe GitObjectType
objectTypeP = parseMaybe (choice typeParser)
  where
    types = [minBound :: GitObjectType .. maxBound]
    typeParser = map (\t -> t <$ string (show t) :: Parsec Void String GitObjectType) types