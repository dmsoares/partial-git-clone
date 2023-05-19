module Core.Commands where

import Core.Actions
import Core.Object

data Command
  = Init FilePath
  | CatFile GitObjectType SHA
  | HashObject GitObjectType Write FilePath
  | Log SHA
  | LsTree SHA

dispatchCommand :: Command -> IO ()
dispatchCommand (Init path) = runInit (initAction path)
dispatchCommand (CatFile _ object) = runWithRepo $ catFileAction object
dispatchCommand (HashObject typ w path) = runWithRepo $ hashObjectAction typ w path
dispatchCommand (Log sha) = runWithRepo $ logAction sha
dispatchCommand (LsTree sha) = runWithRepo $ lsTree sha