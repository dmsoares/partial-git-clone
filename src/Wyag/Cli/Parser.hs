module Wyag.Cli.Parser where

import Data.Void
import qualified Options.Applicative as Opt
import Text.Megaparsec
import Text.Megaparsec.Char
import Wyag.Core.Commands
import Wyag.Core.Object hiding (objectTypeP)
import Prelude hiding (init)

commandParser :: Opt.Parser Command
commandParser =
  Opt.subparser $
    mconcat
      [ Opt.command "init" (Opt.info initP (Opt.progDesc "Initialize a new, empty repository")),
        Opt.command "cat-file" (Opt.info catFileP (Opt.progDesc "Provide content of repository objects")),
        Opt.command "hash-object" (Opt.info hashObjectP (Opt.progDesc "Compute object ID and optionally creates a blob from a file")),
        Opt.command "log" (Opt.info logP (Opt.progDesc "Shows the commit logs")),
        Opt.command "ls-tree" (Opt.info lsTreeP (Opt.progDesc "List the contents of a tree object")),
        Opt.command "checkout" (Opt.info checkoutP (Opt.progDesc "Checkout a commit inside of a directory")),
        Opt.command "show-ref" (Opt.info showRefP (Opt.progDesc "List references in a local repository")),
        Opt.command "tag" (Opt.info tagP (Opt.progDesc "Create or list a tag object"))
      ]

initP :: Opt.Parser Command
initP = InitCommand <$> Opt.strArgument (Opt.metavar "path" <> Opt.value "." <> Opt.help "Where to create the repository")

catFileP :: Opt.Parser Command
catFileP =
  CatFileCommand
    <$> Opt.argument (Opt.maybeReader objectTypeP) (Opt.metavar "type" <> Opt.help "Specify the type")
    <*> Opt.strArgument (Opt.metavar "object" <> Opt.help "The object to display")

hashObjectP :: Opt.Parser Command
hashObjectP =
  HashObjectCommand
    <$> Opt.option (Opt.maybeReader objectTypeP) (Opt.long "type" <> Opt.short 't' <> Opt.metavar "TYPE" <> Opt.value BlobType <> Opt.help "Specify the type")
    <*> Opt.switch (Opt.long "write" <> Opt.short 'w' <> Opt.help "Write object into database")
    <*> Opt.strArgument (Opt.metavar "path" <> Opt.help "Read object from <file>")

logP :: Opt.Parser Command
logP = LogCommand <$> Opt.strArgument (Opt.metavar "commit" <> Opt.help "The commit to log")

lsTreeP :: Opt.Parser Command
lsTreeP = LsTreeCommand <$> Opt.strArgument (Opt.metavar "object" <> Opt.help "The tree to show")

checkoutP :: Opt.Parser Command
checkoutP =
  CheckoutCommand
    <$> Opt.strArgument (Opt.metavar "commit" <> Opt.help "The commit or tree to checkout")
    <*> Opt.strArgument (Opt.metavar "path" <> Opt.help "The EMPTY directory to checkout on")

showRefP :: Opt.Parser Command
showRefP = ShowRefCommand <$> Opt.strArgument (Opt.metavar "path" <> Opt.value "" <> Opt.help "The path to refs")

tagP :: Opt.Parser Command
tagP =
  TagCreateCommand
    <$> Opt.switch (Opt.short 'a' <> Opt.help "Whether to create a tag object")
    <*> ( (,,"Virginia","Jun 01 2023","A comment")
            <$> Opt.strArgument (Opt.metavar "object" <> Opt.help "The object to tag")
            <*> Opt.strArgument (Opt.metavar "name" <> Opt.help "The tag name")
        )
    <|> pure TagListCommand

objectTypeP :: String -> Maybe GitObjectType
objectTypeP = parseMaybe (choice typeParser)
  where
    types = [minBound :: GitObjectType .. maxBound]
    typeParser = map (\t -> t <$ string (show t) :: Parsec Void String GitObjectType) types