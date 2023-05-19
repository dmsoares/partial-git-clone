module Core.Exceptions where

import Control.Exception

data GitException
  = NotADirectory
  | NotAGitRepository
  | DirectoryNotEmpty
  | ConfigFileMissing
  | NoGitDirectory
  | NotAGitObject
  | NotAGitCommitObject
  | MalformedConfig String
  deriving (Eq, Show)

instance Exception GitException
