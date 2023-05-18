module Core.Object where

import Codec.Compression.Zlib (compress, decompress)
import Control.Monad
import Core.Commit
import Core.Parser
import Core.Repo
import Crypto.Hash.SHA1 (hash)
import Data.ByteString as B
import Data.ByteString.Base16
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8
import Data.Byteable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import System.FilePath
import qualified Text.Megaparsec.Byte.Lexer as L

type SHA = Text

data GitObjectType = BlobType | CommitType | TagType | TreeType
  deriving (Bounded, Enum, Show)

data GitObject
  = GitBlob Blob
  | GitCommit Commit
  | GitTag Tag
  | GitTree Tree
  deriving (Show)

newtype Blob = Blob ByteString
  deriving (Eq, Show)

newtype Tag = Tag ByteString
  deriving (Eq, Show)

newtype Tree = Tree ByteString
  deriving (Eq, Show)

instance Byteable GitObjectType where
  toBytes = fromString . show

instance Byteable Blob where
  toBytes (Blob contents) = contents

instance Byteable Tag where
  toBytes (Tag contents) = contents

instance Byteable Tree where
  toBytes (Tree contents) = contents

instance Byteable GitObject where
  toBytes (GitBlob blob) = withHeader "blob" $ toBytes blob
  toBytes (GitCommit commit) = withHeader "commit" $ toBytes commit
  toBytes (GitTag tag) = withHeader "tag" $ toBytes tag
  toBytes (GitTree tree) = withHeader "tree" $ toBytes tree

withHeader :: ByteString -> ByteString -> ByteString
withHeader oType content = mconcat [oType, " ", fromString . show $ B.length content, "\NUL", content]

readObject :: RepoMetadata -> Text -> IO (Maybe GitObject)
readObject repo sha
  | T.length sha < 2 = pure Nothing
  | otherwise = do
      let path = mkRepoPath repo $ "objects" </> T.unpack (T.take 2 sha) </> T.unpack (T.drop 2 sha)
      file <- BL.toStrict . decompress <$> BL.readFile path
      pure $ parseMaybe objectP file

writeObject :: RepoMetadata -> GitObject -> IO SHA
writeObject repo obj = do
  let (sha, result) = BL.toStrict . compress . BL.fromStrict <$> hashObject obj
  writeSerializedObject repo sha result

writeSerializedObject :: RepoMetadata -> SHA -> ByteString -> IO SHA
writeSerializedObject repo sha bytes = do
  let dir = "objects" </> T.unpack (T.take 2 sha)
      fname = T.drop 2 sha
  createRepositoryDirectory repo True dir
  createRepositoryFile repo (dir </> T.unpack fname) bytes
  pure sha

hashObject :: GitObject -> (SHA, ByteString)
hashObject obj =
  let serializedObject = toBytes obj
      sha = genSHA serializedObject
   in (sha, serializedObject)

genSHA :: ByteString -> SHA
genSHA = decodeUtf8 . encode . hash

fromContents :: ByteString -> GitObjectType -> ByteString
fromContents contents typ = withHeader (toBytes typ) contents

-- Object parsers

headerP :: Parser (ByteString, Int)
headerP = do
  typ <- takeWhileP (Just "object type") (/= 20)
  space
  size <- L.decimal
  void $ char 0
  pure (typ, size)

objectP :: Parser GitObject
objectP = do
  (typ, size) <- headerP
  contents <- objectContentsP

  if fromIntegral size /= B.length contents
    then fail $ "Malformed object " ++ show typ ++ ": bad length"
    else case typ of
      "blob" -> GitBlob . Blob <$> takeRest
      "commit" -> GitCommit <$> commitP
      "tag" -> GitTag . Tag <$> takeRest
      "tree" -> GitTree . Tree <$> takeRest
      _ -> fail "No such type"

objectContentsP :: Parser ByteString
objectContentsP = takeRest

nullP :: Parser ()
nullP = void $ char 0

parseFileTest :: (Show a) => Parser a -> FilePath -> IO a
parseFileTest parser path = do
  compressedFile <- BL.readFile path
  let decompressedFile = BL.toStrict . decompress $ compressedFile
      Just parsed = parseMaybe parser decompressedFile
  print decompressedFile
  print parsed
  pure parsed
