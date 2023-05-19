module Core.Tree where

import Control.Monad
import Core.Parser
import Data.ByteString as B
import Data.Byteable

data TreeEntry = TreeEntry {mode :: ByteString, path :: ByteString, sha :: ByteString}
  deriving (Eq, Show)

instance Byteable TreeEntry where
  toBytes (TreeEntry {..}) = mode <> " " <> path <> "\NUL" <> sha

newtype Tree = Tree {treeEntries :: [TreeEntry]}
  deriving (Eq, Show)

instance Byteable Tree where
  toBytes (Tree entries) = intercalate "\n" $ toBytes <$> entries

treeEntryP :: Parser TreeEntry
treeEntryP =
  TreeEntry
    <$> (B.pack <$> (some digitChar <* space))
    <*> (B.pack <$> (some printChar <* char 00))
    <*> (B.pack <$> some hexDigitChar <* (void eol <|> eof))

treeP :: Parser Tree
treeP = Tree <$> many treeEntryP