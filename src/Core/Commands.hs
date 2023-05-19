module Core.Commands where

import Control.Monad
import Control.Monad.RWS
import Control.Monad.Reader (ReaderT (runReaderT))
import Core.Commit
import Core.Exceptions (GitException (NotAGitCommitObject, NotAGitObject, NotAGitRepository))
import Core.Object
import Core.Repo (RepoMetadata, emptyRepoMetadata, findRepo, initRepository)
import qualified Data.ByteString as B
import Data.Byteable
import Data.Foldable

type Write = Bool

type GitAction = ReaderT RepoMetadata IO ()

runWithRepo :: GitAction -> IO ()
runWithRepo action = do
  mRepo <- findRepo "." True
  case mRepo of
    Nothing -> print NotAGitRepository
    Just repo -> runReaderT action repo

data Command
  = Init FilePath
  | CatFile GitObjectType SHA
  | HashObject GitObjectType Write FilePath
  | Log SHA

dispatchCommand :: Command -> IO ()
dispatchCommand (Init path) = runReaderT (initAction path) emptyRepoMetadata
dispatchCommand (CatFile _ object) = runWithRepo $ catFileAction object
dispatchCommand (HashObject typ w path) = runWithRepo $ hashObjectAction typ w path
dispatchCommand (Log sha) = runWithRepo $ logAction sha

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
logAction sha = do
  repo <- ask
  mObject <- lift $ readObject repo sha
  case mObject of
    Nothing -> lift $ print NotAGitObject
    Just object -> do
      case object of
        GitCommit commit@(Commit {..}) -> do
          lift $ print $ toBytes commit <> "\n---\n"
          traverse_ logAction commitParents
        _ -> lift $ print NotAGitCommitObject
