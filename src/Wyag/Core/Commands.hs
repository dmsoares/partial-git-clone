module Wyag.Core.Commands where

import Wyag.Core.Actions
import Wyag.Core.Object

data Command
  = Init FilePath
  | CatFile GitObjectType SHA
  | HashObject GitObjectType Write FilePath
  | Log SHA
  | LsTree SHA
  | Checkout SHA FilePath
  | ShowRef FilePath

dispatchCommand :: Command -> IO ()
dispatchCommand (Init path) = runInit $ initAction path
dispatchCommand (CatFile _ object) = runWithRepo $ catFileAction object
dispatchCommand (HashObject typ w path) = runWithRepo $ hashObjectAction typ w path
dispatchCommand (Log sha) = runWithRepo $ logAction sha
dispatchCommand (LsTree sha) = runWithRepo $ lsTreeAction sha
dispatchCommand (Checkout sha path) = runWithRepo $ checkoutAction path sha
dispatchCommand (ShowRef path)
  | "" <- path = runWithRepo $ showRefAction Nothing
  | otherwise = runWithRepo $ showRefAction (Just path)