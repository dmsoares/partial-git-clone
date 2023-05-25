module Wyag.Core.Parser (module P, Parser, parseFileTest, parseFileTest', readWriteTest) where

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

parseFileTest' :: (Show a) => Parser a -> FilePath -> IO ()
parseFileTest' parser path = do
  file <- B.readFile path
  let parsed = parse parser "file" file

  print $ "file: " <> file
  print parsed

readWriteTest :: ByteString -> FilePath -> IO ()
readWriteTest contents path = do
  B.writeFile path contents
  file <- B.readFile path
  print file
