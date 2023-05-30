module Wyag.Core.Actions where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString as B
import Data.ByteString.UTF8
import Data.Byteable
import Data.Foldable
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>))
import Wyag.Core.Commit
import Wyag.Core.Exceptions
import Wyag.Core.Object
import Wyag.Core.Ref
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
    Nothing -> throw NotAGitRepository
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
logAction = useCommit logCommit
  where
    logCommit commit@(Commit {..}) = do
      lift $ print $ toBytes commit <> "\n---\n"
      traverse_ logAction commitParents

lsTreeAction :: SHA -> GitAction ()
lsTreeAction = useTree lsTree
  where
    lsTree (Tree {treeEntries}) = traverse_ printEntry treeEntries
    printEntry (TreeEntry {mode, path, sha}) =
      useObject
        (\obj -> lift $ print (mode <> " " <> sha <> " " <> (fromString . show $ objectType obj) <> " " <> path))
        sha

checkoutAction :: FilePath -> SHA -> GitAction ()
checkoutAction path sha = do
  isSafe <- lift $ isDirectoryEmpty path
  if isSafe
    then useCommit checkoutCommit sha
    else throw DirectoryNotEmpty
  where
    checkoutCommit (Commit {commitTree}) = useTree checkoutTree commitTree
    checkoutTree (Tree entries) = traverse_ (writeTreeEntry path) entries

showRefAction :: Maybe FilePath -> GitAction ()
showRefAction path = do
  gitdir <- asks gitdir
  let path' = fromMaybe gitdir path </> "refs"

  refs <- lift $ newIORef (M.empty :: M.Map FilePath ByteString)

  collectRefs refs path'
  printRefs refs
  where
    collectRefs refs path' = do
      files <- lift (sort <$> listDirectory path')
      for_ files \file -> do
        isDir <- lift $ doesDirectoryExist (path' </> file)
        if isDir
          then collectRefs refs (path' </> file)
          else do
            ref <- lift $ resolveRef (T.pack path') (T.pack file)
            lift $ modifyIORef refs $ M.insert (path' </> file) (toBytes ref)
    printRefs refs =
      lift (readIORef refs) >>= \(M.toList -> refs') -> do
        lift $ for_ refs' (\(k, v) -> putStrLn (toString v <> " " <> k))

-- GitAction Utils
askObject :: SHA -> GitAction (Maybe GitObject)
askObject sha = do
  repo <- ask
  lift $ readObject repo sha

useObject :: (GitObject -> GitAction ()) -> SHA -> GitAction ()
useObject f sha = do
  mObject <- askObject sha
  case mObject of
    Nothing -> throw NotAGitObject
    Just object -> f object

useCommit :: (Commit -> GitAction ()) -> SHA -> GitAction ()
useCommit action = useObject dispatcher
  where
    dispatcher (GitCommit commit) = action commit
    dispatcher _ = throw (GitObjectTypeMismatch "commit")

useTree :: (Tree -> GitAction ()) -> SHA -> GitAction ()
useTree action = useObject dispatcher
  where
    dispatcher (GitTree tree) = action tree
    dispatcher _ = throw (GitObjectTypeMismatch "tree")

writeTreeEntry :: FilePath -> TreeEntry -> GitAction ()
writeTreeEntry currentPath (TreeEntry {path, sha}) = do
  mObject <- askObject sha
  let newPath = currentPath </> toString path
  case mObject of
    Just obj -> maybe throwException ($ newPath) (writeBlob obj <|> writeTree obj)
    _ -> throwException
  where
    throwException = throw $ GitObjectTypeMismatch "tree | blob"

writeBlob :: GitObject -> Maybe (FilePath -> GitAction ())
writeBlob (GitBlob (Blob contents)) = Just (\path -> lift $ B.writeFile path contents)
writeBlob _ = Nothing

writeTree :: GitObject -> Maybe (FilePath -> GitAction ())
writeTree (GitTree (Tree entries)) = Just $ \path -> do
  lift $ createDirectoryIfMissing True path
  traverse_ (writeTreeEntry path) entries
writeTree _ = Nothing
