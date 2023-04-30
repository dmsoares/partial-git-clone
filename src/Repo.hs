module Repo where

import Config
import Control.Monad
import Gitdir
import System.Directory
import System.FilePath

data Repo = Repo
  { worktree :: FilePath,
    gitdir :: Gitdir,
    cfg :: Config
  }

repoPath :: Repo -> FilePath -> FilePath
repoPath (Repo {gitdir}) fp = let gd = dir gitdir in gd </> fp

repoDir :: Repo -> FilePath -> Maybe Bool -> IO (Maybe FilePath)
repoDir repo fp mkdir = do
  let path = repoPath repo fp

  exists <- doesPathExist path

  if exists
    then do
      isDir <- doesDirectoryExist path

      if isDir
        then do
          return $ Just path
        else return Nothing
    else do
      case mkdir of
        Nothing -> return Nothing
        Just isMkdir ->
          if isMkdir
            then do
              createDirectory path
              return $ Just path
            else return Nothing
