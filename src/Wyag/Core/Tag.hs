module Wyag.Core.Tag where

import Data.ByteString as B
import Data.Byteable
import Wyag.Core.Parser

data Tag = Tag
  { tagTagger :: ByteString,
    tagDate :: ByteString,
    tagMessage :: ByteString
  }
  deriving (Eq, Show)

instance Byteable Tag where
  toBytes (Tag {..}) =
    mconcat
      [ "tagger " <> tagTagger <> "\n",
        "date " <> tagDate <> "\n",
        "\n" <> tagMessage <> "\n"
      ]

tagP :: Parser Tag
tagP =
  Tag
    <$> kvP "tagger"
    <*> kvP "date"
    <*> (newline >> takeRest)

kvP :: ByteString -> Parser ByteString
kvP key = B.pack <$> (string key *> space *> many printChar <* eol)
