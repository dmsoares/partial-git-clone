module Core.Commands where

data Command
  = Init FilePath
  | Read FilePath
  deriving (Show)
