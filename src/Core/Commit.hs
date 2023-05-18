module Core.Commit where

import Data.ByteString.Lazy as BL
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Byte

data CommitContent = CommitContent
  { commitTree :: ByteString,
    commitParents :: [ByteString],
    commitAuthor :: ByteString,
    commitCommitter :: ByteString,
    commitMessage :: ByteString
  }
  deriving (Eq, Show)

type Parser = Parsec Void ByteString

commitP :: Parser CommitContent
commitP =
  CommitContent
    <$> kvP "tree"
    <*> many (kvP "parent")
    <*> kvP "author"
    <*> kvP "committer"
    <*> (newline >> takeRest)

kvP :: ByteString -> Parser ByteString
kvP key = BL.pack <$> (string key *> space *> many printChar <* eol)

parseFileTest :: (Show a) => Parser a -> FilePath -> IO ()
parseFileTest parser path = do
  file <- BL.readFile path
  parseTest parser file
