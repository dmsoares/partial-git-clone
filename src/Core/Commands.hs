{-# LANGUAGE RankNTypes #-}

module Core.Commands where

import Control.Monad
import Core.Object
import Core.Repo (findRepo, generateRepositoryMetadata, initRepository)
import qualified Data.ByteString as B
import Data.Text

type Write = Bool

type ObjectSha = Text

data Command
  = Init FilePath
  | CatFile GitObjectType ObjectSha
  | HashObject GitObjectType Write FilePath

initAction :: FilePath -> IO ()
initAction = initRepository

catFileAction :: ObjectSha -> IO ()
catFileAction sha = do
  Just repo <- findRepo "." True
  Just obj <- readObject repo sha
  print obj

hashObjectAction :: GitObjectType -> Write -> FilePath -> IO ()
hashObjectAction typ w path = do
  contents <- B.readFile path
  let obj = fromContents contents typ
      sha = genSHA obj
  when w do
    repo <- generateRepositoryMetadata "." False
    void $ writeSerializedObject repo sha obj
  print sha
