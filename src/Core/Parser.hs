module Core.Parser (module P, Parser) where

import Data.ByteString as B
import Data.Void
import Text.Megaparsec as P
import Text.Megaparsec.Byte as P

type Parser = Parsec Void ByteString