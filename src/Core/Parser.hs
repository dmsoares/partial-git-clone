module Core.Parser (module P, Parser, parseFileTest) where

import Codec.Compression.Zlib (decompress)
import Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Void
import Text.Megaparsec as P
import Text.Megaparsec.Byte as P

type Parser = Parsec Void ByteString

parseFileTest :: (Show a) => Parser a -> FilePath -> IO ()
parseFileTest parser path = do
  compressedFile <- BL.readFile path
  let decompressedFile = BL.toStrict . decompress $ compressedFile
      Just parsed = parseMaybe parser decompressedFile
  print decompressedFile
  print parsed