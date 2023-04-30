module Commands where

newtype Command
  = Init FilePath
  deriving (Show)
