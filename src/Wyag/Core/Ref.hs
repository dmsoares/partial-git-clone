module Wyag.Core.Ref where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import System.FilePath
import Wyag.Core.Parser
import Wyag.Core.Repo

newtype Ref = Ref ByteString

resolveRef :: RepoMetadata -> Text -> IO Ref
resolveRef repo@(RepoMetadata {gitdir}) refName =
  let refP = string "ref: " *> takeRest :: Parser ByteString
   in do
        contents <- B.readFile (gitdir </> T.unpack refName)
        let result = parse refP "ref" contents
        case result of
          Left _ -> pure $ Ref contents
          Right newRefName -> resolveRef repo (decodeUtf8 newRefName)