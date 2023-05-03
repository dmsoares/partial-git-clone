module Repo where

import Config
import Control.Exception
import Control.Monad.Extra
import Data.Foldable
import Data.Function
import Exceptions
import System.Directory
import System.FilePath
import System.IO.Error
import Utils

data RepoMetadata = RepoMetadata
  { worktree :: FilePath,
    gitdir :: FilePath,
    cfg :: Config
  }
  deriving (Show)

-- | Prefixes a filepath with the repository gitdir path
mkRepoPath :: RepoMetadata -> FilePath -> FilePath
mkRepoPath (RepoMetadata {gitdir}) fp = let gd = gitdir in gd </> fp

mkGitdir :: FilePath -> FilePath
mkGitdir fp = fp </> ".git"

-- | Creates a directory inside the repository's gitdir,
-- optionally creating parents
createRepoDir :: RepoMetadata -> Bool -> FilePath -> IO ()
createRepoDir repo createParents fp = do
  let path = mkRepoPath repo fp
  createDirectoryIfMissing createParents path

createRepositoryMetadata :: FilePath -> Bool -> IO RepoMetadata
createRepositoryMetadata worktreePath force = do
  let gitdir = mkGitdir worktreePath
  isDirectory <- doesDirectoryExist gitdir

  if not (force || isDirectory)
    then throw NotAGitRepository
    else do
      let configPath = gitdir </> "config"
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
  metadata <- createRepositoryMetadata path True
  traverse_
    (createRepoDir metadata True)
    ["branches", "objects", "refs" </> "tags", "refs" </> "heads"]
  writeFile (mkRepoPath metadata "description") "Unnamed repository; edit this file 'description' to name the repository.\n"
  writeFile (mkRepoPath metadata "HEAD") "ref: refs/heads/master\n"
  writeConfig (mkRepoPath metadata "config")

findRepo :: FilePath -> Bool -> IO (Maybe RepoMetadata)
findRepo path required = do
  path' <- getRealPath path
  result <- createRepositoryMetadata path' False & tryJust (guard . (== NotAGitRepository))
  case result of
    Right repoMetadata -> pure $ Just repoMetadata
    Left _ -> do
      parent <- getSymbolicLinkTarget $ path' </> ".."
      if parent == path'
        then if required then throw NoGitDirectory else pure Nothing
        else findRepo parent required

-- | If path is symlink, then return target, else path
getRealPath :: FilePath -> IO FilePath
getRealPath fp = ifM (pathIsSymbolicLink fp) (getSymbolicLinkTarget fp) (pure fp)

-- | Get .git directory in filepath, or Nothing if it's not there
getGitdir :: FilePath -> IO (Maybe FilePath)
getGitdir fp =
  ifM (doesDirectoryExist gitdir) (pure $ Just gitdir) (pure Nothing)
  where
    gitdir = mkGitdir fp
