module Core.Actions where

import Control.Monad
import Control.Monad.Reader
import Core.Commit
import Core.Exceptions
import Core.Object
import Core.Repo
import Core.Tree
import qualified Data.ByteString as B
import Data.ByteString.UTF8
import Data.Byteable
import Data.Foldable

type GitAction = ReaderT RepoMetadata IO ()

type Write = Bool

runInit :: GitAction -> IO ()
runInit = flip runReaderT emptyRepoMetadata

runWithRepo :: GitAction -> IO ()
runWithRepo action = do
  mRepo <- findRepo "." True
  case mRepo of
    Nothing -> print NotAGitRepository
    Just repo -> runReaderT action repo

initAction :: FilePath -> GitAction
initAction = lift . initRepository

catFileAction :: SHA -> GitAction
catFileAction sha = do
  repo <- ask
  Just obj <- lift $ readObject repo sha
  lift $ print obj

hashObjectAction :: GitObjectType -> Write -> FilePath -> GitAction
hashObjectAction typ w path = do
  contents <- lift $ B.readFile path
  let obj = fromContents contents typ
      sha = genSHA obj
  when w do
    repo <- ask
    lift $ void $ writeSerializedObject repo sha obj
  lift $ print sha

logAction :: SHA -> GitAction
logAction =
  useObject
    ( \case
        GitCommit commit@(Commit {..}) -> do
          lift $ print $ toBytes commit <> "\n---\n"
          traverse_ logAction commitParents
        _ -> lift $ print (GitObjectTypeMismatch "commit")
    )

lsTree :: SHA -> GitAction
lsTree =
  useObject
    ( \case
        GitTree (Tree {..}) -> do
          traverse_ printEntry treeEntries
        _ -> lift $ print (GitObjectTypeMismatch "tree")
    )
  where
    printEntry (TreeEntry {..}) =
      useObject
        (\obj -> lift $ print (mode <> " " <> sha <> " " <> (fromString . show $ objectType obj) <> "\t" <> path))
        sha

useObject :: (GitObject -> GitAction) -> SHA -> GitAction
useObject f sha = do
  mObject <- do
    repo <- ask
    lift $ readObject repo sha
  case mObject of
    Nothing -> lift $ print NotAGitObject
    Just object -> f object