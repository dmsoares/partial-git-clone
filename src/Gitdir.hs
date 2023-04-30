module Gitdir (Gitdir, dir, mkGitdir) where

import System.FilePath

newtype Gitdir = Gitdir {dir :: FilePath}

mkGitdir :: FilePath -> Gitdir
mkGitdir path = Gitdir $ path </> ".git"