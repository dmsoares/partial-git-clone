module Core.Commands where

import Core.Object
import Data.Text (Text)

data Command
  = Init FilePath
  | CatFile GitObjectType Text
  deriving (Show)
