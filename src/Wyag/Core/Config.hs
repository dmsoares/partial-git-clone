{-# LANGUAGE TemplateHaskell #-}

module Wyag.Core.Config where

import Control.Exception
import Data.Ini.Config.Bidir
import Data.Text
import qualified Data.Text.IO as T
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import System.Directory
import Wyag.Core.Exceptions

data Config = Config
  { _repositoryFormatVersion :: Int,
    _fileMode :: Bool,
    _bare :: Bool
  }
  deriving (Eq, Show)

makeLenses ''Config

defConfig :: Config
defConfig = Config 0 False False

configParser :: IniSpec Config ()
configParser = do
  section "Core" $ do
    repositoryFormatVersion .= field "repositoryformatversion" number
    fileMode .= field "filemode" bool
    bare .= field "bare" bool

configIni :: Ini Config
configIni = ini defConfig configParser

parseConfig :: Text -> Either String Config
parseConfig txt = getIniValue <$> parseIni txt configIni

isValidVersion :: Bool -> Config -> Bool
isValidVersion True _ = True
isValidVersion False cfg =
  case cfg ^. repositoryFormatVersion of
    0 -> True
    _ -> False

readConfig :: Bool -> FilePath -> IO Config
readConfig force path = do
  configExists <- doesFileExist path
  if configExists
    then do
      file <- T.readFile path
      either
        (throw . MalformedConfig)
        return
        (parseConfig file)
    else
      if force
        then return defConfig
        else throw ConfigFileMissing

writeConfig :: FilePath -> IO ()
writeConfig fp = T.writeFile fp $ serializeIni configIni
