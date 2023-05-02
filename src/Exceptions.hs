module Exceptions where

import Control.Exception

data GitException
  = NotADirectory
  | NotAGitRepository
  | DirectoryNotEmpty
  | ConfigFileMissing
  | NoGitDirectory
  | MalformedConfig String
  deriving (Show)

instance Exception GitException
