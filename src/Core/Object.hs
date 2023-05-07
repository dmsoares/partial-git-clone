module Core.Object where

import Codec.Compression.Zlib (compress, decompress)
import Control.Monad
import Core.Repo
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as B
import Data.ByteString.Base16
import Data.ByteString.Lazy as BL hiding (map)
import qualified Data.String as S
import qualified Data.Text as T
import Data.Void
import System.FilePath
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

type SHA = B.ByteString

data GitObject = GitObject {typ :: GitObjectType, size :: Int, contents :: ByteString}
  deriving (Show)

data GitObjectType = Blob | Commit | Tag | Tree
  deriving (Enum, Bounded)

instance Show GitObjectType where
  show Blob = "blob"
  show Commit = "commit"
  show Tag = "tag"
  show Tree = "tree"

readObject :: RepoMetadata -> T.Text -> IO (Maybe GitObject)
readObject repo sha
  | T.length sha < 2 = pure Nothing
  | otherwise = do
      let path = mkRepoPath repo $ "objects" </> T.unpack (T.take 2 sha) </> T.unpack (T.drop 2 sha)
      file <- decompress <$> BL.readFile path
      pure $ parseMaybe objectP file

writeObject :: RepoMetadata -> GitObject -> IO SHA
writeObject repo obj = do
  let (sha, result) = compress <$> hashObject obj
      dir = B.take 2 sha
      fname = B.drop 2 sha
  BL.writeFile (mkRepoPath repo ("objects" </> show dir </> show fname)) result
  pure sha

serializeObject :: GitObject -> ByteString
serializeObject (GitObject {..}) = S.fromString (show typ ++ " " ++ show size ++ ['\NUL']) <> contents

hashObject :: GitObject -> (SHA, ByteString)
hashObject obj =
  let serializedObject = serializeObject obj
      sha = encode . hash . toStrict $ serializedObject
   in (sha, serializedObject)

-- Object parsers
type Parser = Parsec Void ByteString

objectP :: Parser GitObject
objectP = do
  typ <- objectTypeP
  space
  size <- objectSizeP
  nullP
  contents <- objectContentsP

  if fromIntegral size == BL.length contents
    then pure $ GitObject {..}
    else fail $ "Malformed object " ++ show typ ++ ": bad length"

objectTypeP :: Parser GitObjectType
objectTypeP =
  label "valid object type: blob | commit | tag | tree" $
    choice typeParser
  where
    types = [minBound :: GitObjectType .. maxBound]
    typeParser = map (\t -> t <$ string (S.fromString . show $ t)) types

objectSizeP :: Parser Int
objectSizeP = L.decimal

objectContentsP :: Parser ByteString
objectContentsP = takeRest

nullP :: Parser ()
nullP = void $ char 0

parseFileTest :: (Show a) => Parser a -> FilePath -> IO a
parseFileTest parser path = do
  compressedFile <- BL.readFile path
  let decompressedFile = decompress compressedFile
      Just parsed = parseMaybe parser decompressedFile
  print decompressedFile
  print parsed
  pure parsed
