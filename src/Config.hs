{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Bifunctor
import Data.Ini.Config.Bidir
import qualified Data.Text.IO as T
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import System.Directory
import System.IO.Error

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

guardVersion :: Bool -> Config -> Either IOError Config
guardVersion force cfg = do
  let version = cfg ^. repositoryFormatVersion
  if force
    then Right cfg
    else case version of
      0 -> Right cfg
      _ -> Left $ userError $ "Unsupported repositoryformatversion " ++ show version

readConfig :: Bool -> FilePath -> IO (Either IOError Config)
readConfig force path = do
  configExists <- doesFileExist path
  if configExists
    then do
      file <- T.readFile path
      return $ first userError (getIniValue <$> parseIni file configIni)
    else
      if force
        then return $ Right defConfig
        else return $ Left (userError "Configuration file missing")

writeConfig :: FilePath -> IO (Either IOError ())
writeConfig fp = tryIOError $ T.writeFile fp $ serializeIni configIni
