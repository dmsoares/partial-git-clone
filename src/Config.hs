module Config where

import Control.Applicative
import Data.Ini.Config

data CoreConfig = CoreConfig
  { repositoryFormatVersion :: Int,
    fileMode :: Bool,
    bare :: Bool
  }
  deriving (Eq, Show)

newtype Config = Config
  { cfCore :: CoreConfig
  }
  deriving (Eq, Show)

configParser :: IniParser Config
configParser =
  Config
    <$> section
      "CORE"
      ( liftA3
          CoreConfig
          (fieldDefOf "repositoryformatversion" number 0)
          (fieldFlagDef "filemode" False)
          (fieldFlagDef "bare" True)
      )