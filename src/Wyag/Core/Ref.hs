module Wyag.Core.Ref where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Byteable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import System.FilePath
import Wyag.Core.Parser

newtype Ref = Ref ByteString
  deriving (Show)

instance Byteable Ref where
  toBytes (Ref sha) = sha

resolveRef :: Text -> Text -> IO Ref
resolveRef basePath refName =
  let indirectRefP = string "ref: " *> takeRest :: Parser ByteString
   in do
        contents <- B.readFile (T.unpack basePath </> T.unpack refName)
        let result = parse indirectRefP "ref" contents
        case result of
          Left _ -> pure $ Ref (B.takeWhile (/= 10) contents)
          Right newRefName -> resolveRef basePath (decodeUtf8 newRefName)