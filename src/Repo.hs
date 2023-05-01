module Repo where

import Config
import Control.Exception
import Control.Monad.Extra
import Data.Foldable
import Exceptions
import Gitdir
import System.Directory
import System.FilePath
import Utils

data RepoMetadata = RepoMetadata
  { worktree :: FilePath,
    gitdir :: Gitdir,
    cfg :: Config
  }

-- | Prefixes a filepath with the repository gitdir path
mkRepoPath :: RepoMetadata -> FilePath -> FilePath
mkRepoPath (RepoMetadata {gitdir}) fp = let gd = dir gitdir in gd </> fp

-- | Creates a directory inside the repository's gitdir,
-- optionally creating parents
createRepoDir :: RepoMetadata -> Bool -> FilePath -> IO ()
createRepoDir repo createParents fp = do
  let path = mkRepoPath repo fp
  createDirectoryIfMissing createParents path

createNewRepository :: FilePath -> Bool -> IO RepoMetadata
createNewRepository worktreePath force = do
  let gitdir = mkGitdir worktreePath
  isDirectory <- doesDirectoryExist (dir gitdir)

  if not (force || isDirectory)
    then throw NotAGitRepository
    else do
      let configPath = worktreePath </> "config"
      RepoMetadata worktreePath gitdir <$> readConfig force configPath

createWorktreePathIfMissing :: FilePath -> IO ()
createWorktreePathIfMissing fp = do
  exists <- doesPathExist fp
  if exists
    then do
      unlessM (doesDirectoryExist fp) $ throw NotADirectory
      unlessM (isDirectoryEmpty fp) $ throw DirectoryNotEmpty
    else createDirectoryIfMissing True fp

initRepository :: FilePath -> IO ()
initRepository path = do
  createWorktreePathIfMissing path
  metadata <- createNewRepository path True
  traverse_
    (createRepoDir metadata True)
    ["branches", "objects", "refs" </> "tags", "refs" </> "heads"]
  writeFile (mkRepoPath metadata "description") "Unnamed repository; edit this file 'description' to name the repository.\n"
  writeFile (mkRepoPath metadata "HEAD") "ref: refs/heads/master\n"
  writeConfig (mkRepoPath metadata "config")
