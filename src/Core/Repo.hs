module Core.Repo where

import Config
import Control.Exception
import Control.Monad.Extra
import Data.Foldable
import Data.Function
import Exceptions
import System.Directory
import System.FilePath
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
createRepositoryDirectory :: RepoMetadata -> Bool -> FilePath -> IO ()
createRepositoryDirectory repo createParents fp = do
  let path = mkRepoPath repo fp
  createDirectoryIfMissing createParents path

createRepositoryFile :: RepoMetadata -> FilePath -> String -> IO ()
createRepositoryFile metadata path = writeFile (mkRepoPath metadata path)

generateRepositoryMetadata :: FilePath -> Bool -> IO RepoMetadata
generateRepositoryMetadata worktreePath force = do
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
  createInitFilesAndDirectories =<< generateRepositoryMetadata path True

createInitFilesAndDirectories :: RepoMetadata -> IO ()
createInitFilesAndDirectories metadata = traverse_ ($ metadata) [createInitDirectories, createInitFiles]

createInitDirectories :: RepoMetadata -> IO ()
createInitDirectories metadata =
  traverse_
    (createRepositoryDirectory metadata True)
    ["branches", "objects", "refs" </> "tags", "refs" </> "heads"]

createInitFiles :: RepoMetadata -> IO ()
createInitFiles metadata = do
  createRepositoryFile metadata "description" "Unnamed repository; edit this file 'description' to name the repository.\n"
  createRepositoryFile metadata "HEAD" "ref: refs/heads/master\n"
  writeConfig (mkRepoPath metadata "config")

findRepo :: FilePath -> Bool -> IO (Maybe RepoMetadata)
findRepo path required = do
  path' <- getRealPath path
  result <- generateRepositoryMetadata path' False & tryJust (guard . (== NotAGitRepository))
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
