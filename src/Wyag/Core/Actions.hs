module Wyag.Core.Actions where

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as B
import Data.ByteString.UTF8
import Data.Byteable
import Data.Foldable
import Wyag.Core.Commit
import Wyag.Core.Exceptions
import Wyag.Core.Object
import Wyag.Core.Repo
import Wyag.Core.Tree
import Wyag.Core.Utils

type GitAction = ReaderT RepoMetadata IO

type Write = Bool

runInit :: GitAction () -> IO ()
runInit = flip runReaderT emptyRepoMetadata

runWithRepo :: GitAction () -> IO ()
runWithRepo action = do
  mRepo <- findRepo "." True
  case mRepo of
    Nothing -> print NotAGitRepository
    Just repo -> runReaderT action repo

initAction :: FilePath -> GitAction ()
initAction = lift . initRepository

catFileAction :: SHA -> GitAction ()
catFileAction sha = do
  repo <- ask
  Just obj <- lift $ readObject repo sha
  lift $ print obj

hashObjectAction :: GitObjectType -> Write -> FilePath -> GitAction ()
hashObjectAction typ w path = do
  contents <- lift $ B.readFile path
  let obj = fromContents contents typ
      sha = genSHA obj
  when w do
    repo <- ask
    lift $ void $ writeSerializedObject repo sha obj
  lift $ print sha

logAction :: SHA -> GitAction ()
logAction =
  useObject
    ( \case
        GitCommit commit@(Commit {..}) -> do
          lift $ print $ toBytes commit <> "\n---\n"
          traverse_ logAction commitParents
        _ -> lift $ print (GitObjectTypeMismatch "commit")
    )

lsTreeAction :: SHA -> GitAction ()
lsTreeAction =
  useObject
    ( \case
        GitTree (Tree {..}) -> do
          traverse_ printEntry treeEntries
        _ -> lift $ print (GitObjectTypeMismatch "tree")
    )
  where
    printEntry (TreeEntry {..}) =
      useObject
        (\obj -> lift $ print (mode <> " " <> sha <> " " <> (fromString . show $ objectType obj) <> " " <> path))
        sha

checkoutAction :: FilePath -> SHA -> GitAction ()
checkoutAction path sha = do
  isSafe <- lift $ isDirectoryEmpty path
  when isSafe do
    mCommitObj <- askObject sha
    case mCommitObj of
      Nothing -> lift $ putStrLn (show NotAGitObject <> show sha)
      Just (GitCommit (Commit {..})) -> do
        useObject instantiateTree commitTree
      _ -> lift $ print (GitObjectTypeMismatch "commit")
  where
    instantiateTree (GitTree (Tree entries)) = do
      treeExpanded <- execStateT (constructTreeExpanded entries) (TreeNode Nothing [])
      lift $ print treeExpanded
    instantiateTree _ = lift $ print (GitObjectTypeMismatch "tree")

-- | Represents an expanded GitTree, by recursively expanding each sub-tree.
-- Remember that a Git Tree object is only a list of entries (either sub-trees or blobs).
-- In a TreeExpanded, nodes are sub-trees, leaves are blobs: the top-most tree represents
-- the worktree and thus has no associated TreeEntry (Nothing)
data TreeExpanded = TreeNode (Maybe TreeEntry) [TreeExpanded] | TreeLeaf TreeEntry
  deriving (Show)

constructTreeExpanded :: [TreeEntry] -> StateT TreeExpanded GitAction ()
constructTreeExpanded entries = do
  treeExpanded <- get
  case treeExpanded of
    TreeLeaf _ -> pure ()
    TreeNode e _ -> do
      trees <- traverse expandNode entries
      put $ TreeNode e trees
  where
    expandNode entry@(TreeEntry {sha}) = do
      mObject <- lift $ askObject sha
      case mObject of
        Just (GitBlob _) -> pure $ TreeLeaf entry
        Just (GitTree (Tree entries')) -> do
          subTrees <- traverse expandNode entries'
          pure $ TreeNode (Just entry) subTrees
        _ -> throw $ GitObjectTypeMismatch "tree | blob"

askObject :: SHA -> GitAction (Maybe GitObject)
askObject sha = do
  repo <- ask
  lift $ readObject repo sha

useObject :: (GitObject -> GitAction ()) -> SHA -> GitAction ()
useObject f sha = do
  mObject <- askObject sha
  case mObject of
    Nothing -> lift $ print NotAGitObject
    Just object -> f object