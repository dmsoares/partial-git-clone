module Core.Object where

import Codec.Compression.Zlib (compress, decompress)
import Core.Commit
import Core.Parser
import Core.Repo
import Core.Tree
import Crypto.Hash.SHA1 (hash)
import Data.ByteString as B
import Data.ByteString.Base16
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8
import Data.Byteable
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import System.FilePath
import qualified Text.Megaparsec.Byte.Lexer as L

type SHA = ByteString

data GitObjectType = BlobType | CommitType | TagType | TreeType
  deriving (Bounded, Enum)

instance Show GitObjectType where
  show BlobType = "blob"
  show CommitType = "commit"
  show TagType = "tag"
  show TreeType = "tree"

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

instance Byteable GitObjectType where
  toBytes = fromString . show

instance Byteable Blob where
  toBytes (Blob contents) = contents

instance Byteable Tag where
  toBytes (Tag contents) = contents

instance Byteable GitObject where
  toBytes (GitBlob blob) = withHeader "blob" $ toBytes blob
  toBytes (GitCommit commit) = withHeader "commit" $ toBytes commit
  toBytes (GitTag tag) = withHeader "tag" $ toBytes tag
  toBytes (GitTree tree) = withHeader "tree" $ toBytes tree

withHeader :: ByteString -> ByteString -> ByteString
withHeader oType content = mconcat [oType, " ", fromString . show $ B.length content, "\NUL", content]

readObject :: RepoMetadata -> SHA -> IO (Maybe GitObject)
readObject repo sha
  | B.length sha < 2 = pure Nothing
  | otherwise = do
      let path = mkRepoPath repo $ "objects" </> T.unpack (decodeUtf8 (B.take 2 sha)) </> T.unpack (decodeUtf8 (B.drop 2 sha))
      file <- BL.toStrict . decompress <$> BL.readFile path
      pure $ parseMaybe objectP file

writeObject :: RepoMetadata -> GitObject -> IO SHA
writeObject repo obj = do
  let (sha, result) = hashObject obj
  writeSerializedObject repo sha result

writeSerializedObject :: RepoMetadata -> SHA -> ByteString -> IO SHA
writeSerializedObject repo sha bytes = do
  let dir = "objects" </> T.unpack (decodeUtf8 (B.take 2 sha))
      fname = decodeUtf8 $ B.drop 2 sha
      compressedBytes = BL.toStrict . compress . BL.fromStrict $ bytes
  createRepositoryDirectory repo True dir
  createRepositoryFile repo (dir </> T.unpack fname) compressedBytes
  pure sha

hashObject :: GitObject -> (SHA, ByteString)
hashObject obj =
  let serializedObject = toBytes obj
      sha = genSHA serializedObject
   in (sha, serializedObject)

genSHA :: ByteString -> SHA
genSHA = encode . hash

fromContents :: ByteString -> GitObjectType -> ByteString
fromContents contents typ = withHeader (toBytes typ) contents

objectType :: GitObject -> GitObjectType
objectType (GitBlob _) = BlobType
objectType (GitCommit _) = CommitType
objectType (GitTag _) = TagType
objectType (GitTree _) = TreeType

-- Object parsers

objectP :: Parser GitObject
objectP = do
  (typ, size) <- headerP
  contents <- lookAhead objectContentsP

  if fromIntegral size /= B.length contents
    then fail $ "Malformed object " ++ show typ ++ ": bad length"
    else case typ of
      BlobType -> pure . GitBlob . Blob $ contents
      CommitType -> GitCommit <$> commitP
      TagType -> pure . GitTag . Tag $ contents
      TreeType -> GitTree <$> treeP

headerP :: Parser (GitObjectType, Int)
headerP = (,) <$> (objectTypeP <* space) <*> (L.decimal <* char 0)

objectTypeP :: Parser GitObjectType
objectTypeP =
  label "valid object type: blob | commit | tag | tree" $
    choice typeParser
  where
    types = [minBound :: GitObjectType .. maxBound]
    typeParser = fmap (\t -> t <$ string (toBytes t)) types

objectContentsP :: Parser ByteString
objectContentsP = takeRest
