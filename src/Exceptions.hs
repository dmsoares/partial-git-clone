module Exceptions where

import Control.Exception

data GitException
  = NotADirectory
  | NotAGitRepository
  | DirectoryNotEmpty
  | ConfigFileMissing
  | MalformedConfig String
  deriving (Show)

instance Exception GitException
