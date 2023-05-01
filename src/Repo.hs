module Repo where

import Config
import Control.Monad
import Data.Either
import Gitdir
import System.Directory
import System.FilePath
import System.IO.Error

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
createRepoDir :: RepoMetadata -> Bool -> FilePath -> IO (Either IOError FilePath)
createRepoDir repo createParents fp = do
  let path = mkRepoPath repo fp
  result <- tryIOError $ createDirectoryIfMissing createParents path
  return (path <$ result)

createNewRepository :: FilePath -> Bool -> IO (Either IOError RepoMetadata)
createNewRepository worktreePath force = do
  let gitdir = mkGitdir worktreePath
  isDirectory <- doesDirectoryExist (dir gitdir)

  if not (force || isDirectory)
    then return $ Left (userError "Not a Git repository")
    else do
      let configPath = worktreePath </> "config"
      eitherConfig <- readConfig force configPath
      return $ RepoMetadata worktreePath gitdir <$> (eitherConfig >>= guardVersion force)

createWorktreePathIfMissing :: FilePath -> IO (Either IOError ())
createWorktreePathIfMissing fp = do
  exists <- doesPathExist fp
  if exists
    then do
      isDir <- doesDirectoryExist fp
      if not isDir
        then return $ Left $ userError $ show fp ++ " is not a directory!"
        else do
          contents <- listDirectory fp
          if not (null contents)
            then return $ Left $ userError $ show fp ++ " is not empty!"
            else return $ Right ()
    else tryIOError $ createDirectoryIfMissing True fp

initRepository :: FilePath -> IO (Either IOError ())
initRepository path = do
  result <- createWorktreePathIfMissing path
  case result of
    Left err -> return $ Left err
    Right _ -> do
      eMetadata <- createNewRepository path True
      case eMetadata of
        Left err -> return $ Left err
        Right metadata -> do
          createdDirs <-
            traverse
              (createRepoDir metadata True)
              ["branches", "objects", "refs" </> "tags", "refs" </> "heads"]
          if not . null . lefts $ createdDirs
            then return $ void (head createdDirs)
            else do
              writeFile (mkRepoPath metadata "description") "Unnamed repository; edit this file 'description' to name the repository.\n"
              writeFile (mkRepoPath metadata "HEAD") "ref: refs/heads/master\n"
              writeConfig (mkRepoPath metadata "config")
