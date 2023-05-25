module Wyag.Core.Utils where

import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as B
import Data.ByteString.Base16
import System.Directory

isDirectoryEmpty :: FilePath -> IO Bool
isDirectoryEmpty fp = null <$> listDirectory fp

genSHA :: B.ByteString -> B.ByteString
genSHA = encode . hash
