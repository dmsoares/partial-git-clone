module Wyag.Core.Commands where

import Data.ByteString
import Wyag.Core.Actions
import Wyag.Core.Object
import Wyag.Core.Ref
import Wyag.Core.Tag

data Command
  = InitCommand FilePath
  | CatFileCommand GitObjectType SHA
  | HashObjectCommand GitObjectType Write FilePath
  | LogCommand SHA
  | LsTreeCommand SHA
  | CheckoutCommand SHA FilePath
  | ShowRefCommand FilePath
  | TagCreateCommand Bool (ByteString, ByteString, ByteString, ByteString, ByteString)
  | TagListCommand

dispatchCommand :: Command -> IO ()
dispatchCommand (InitCommand path) = runInit $ initAction path
dispatchCommand (CatFileCommand _ object) = runWithRepo $ catFileAction object
dispatchCommand (HashObjectCommand typ w path) = runWithRepo $ hashObjectAction typ w path
dispatchCommand (LogCommand sha) = runWithRepo $ logAction sha
dispatchCommand (LsTreeCommand sha) = runWithRepo $ lsTreeAction sha
dispatchCommand (CheckoutCommand sha path) = runWithRepo $ checkoutAction path sha
dispatchCommand (ShowRefCommand path)
  | "" <- path = runWithRepo $ showRefAction Nothing
  | otherwise = runWithRepo $ showRefAction (Just path)
dispatchCommand (TagCreateCommand isObjectTag (object, name, tagger, date, comment)) =
  if isObjectTag
    then runWithRepo . tagCreateAction . Right . mkTag $ (object, name, tagger, date, comment)
    else runWithRepo . tagCreateAction . Left $ (name, Ref object)
  where
    mkTag (a, b, c, d, e) = Tag a b c d e
dispatchCommand TagListCommand = runWithRepo tagListAction