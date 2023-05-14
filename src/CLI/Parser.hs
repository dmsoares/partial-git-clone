module CLI.Parser where

import Core.Commands
import Core.Object hiding (Parser, objectTypeP)
import Data.Void
import qualified Options.Applicative as Opt
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (init)

commandParser :: Opt.Parser Command
commandParser =
  Opt.subparser $
    mconcat
      [ Opt.command "init" (Opt.info initP (Opt.progDesc "Initialize a new, empty repository")),
        Opt.command "cat-file" (Opt.info catFileP (Opt.progDesc "Provide content of repository objects")),
        Opt.command "hash-object" (Opt.info hashObjectP (Opt.progDesc "Compute object ID and optionally creates a blob from a file"))
      ]

initP :: Opt.Parser Command
initP = Init <$> Opt.strArgument (Opt.metavar "path" <> Opt.value "." <> Opt.help "Where to create the repository")

catFileP :: Opt.Parser Command
catFileP =
  CatFile
    <$> Opt.argument (Opt.maybeReader objectTypeP) (Opt.metavar "type" <> Opt.help "Specify the type")
    <*> Opt.strArgument (Opt.metavar "object" <> Opt.help "The object to display")

hashObjectP :: Opt.Parser Command
hashObjectP =
  HashObject
    <$> Opt.option (Opt.maybeReader objectTypeP) (Opt.long "type" <> Opt.short 't' <> Opt.metavar "TYPE" <> Opt.value Blob <> Opt.help "Specify the type")
    <*> Opt.switch (Opt.long "write" <> Opt.short 'w' <> Opt.help "Write object into database")
    <*> Opt.strArgument (Opt.metavar "path" <> Opt.help "Read object from <file>")

objectTypeP :: String -> Maybe GitObjectType
objectTypeP = parseMaybe (choice typeParser)
  where
    types = [minBound :: GitObjectType .. maxBound]
    typeParser = map (\t -> t <$ string (show t) :: Parsec Void String GitObjectType) types