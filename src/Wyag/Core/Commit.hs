module Wyag.Core.Commit where

import Data.ByteString as B
import Data.Byteable
import Wyag.Core.Parser

data Commit = Commit
  { commitTree :: ByteString,
    commitParents :: [ByteString],
    commitAuthor :: ByteString,
    commitCommitter :: ByteString,
    commitMessage :: ByteString
  }
  deriving (Eq, Show)

mkCommit :: ByteString -> Maybe Commit
mkCommit = parseMaybe commitP

instance Byteable Commit where
  toBytes (Commit {..}) =
    mconcat
      [ "tree " <> commitTree <> "\n",
        mconcat . fmap (\parent -> "parent " <> parent <> "\n") $ commitParents,
        "author " <> commitAuthor <> "\n",
        "committer " <> commitCommitter <> "\n",
        "\n" <> commitMessage <> "\n"
      ]

commitP :: Parser Commit
commitP =
  Commit
    <$> kvP "tree"
    <*> many (kvP "parent")
    <*> kvP "author"
    <*> kvP "committer"
    <*> (newline >> takeRest)

kvP :: ByteString -> Parser ByteString
kvP key = B.pack <$> (string key *> space *> many printChar <* eol)
