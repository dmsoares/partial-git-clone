module Core.Commit where

import Control.Monad
import Data.ByteString.Lazy as BL hiding (foldr)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Void
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte

type SHA = Text

type Header = (ByteString, ByteString)

type Headers = M.Map ByteString [ByteString]
-- ^ The same header may repeat itself (e.g. 'parent'),
-- so we just append values to each key

data KVLM = KVLM
  { headers :: Headers,
    message :: ByteString
  }

type Parser = Parsec Void ByteString

headerP :: Parser Header
headerP = do
  key <- BL.pack <$> some alphaNumChar
  space
  value <- some (toByteStringP printChar <|> toByteStringP (char 20) <|> p')
  void newline
  pure (key, mconcat value)

headersP :: Parser Headers
headersP = collectHeaders <$> many headerP

collectHeaders :: [Header] -> Headers
collectHeaders = foldr f M.empty
  where
    f (k, v) = M.insertWith (++) k [v]

p :: Parser ByteString
p = do
  key <- BL.pack <$> some alphaNumChar
  space
  value <- BL.pack <$> some (alphaNumChar <|> char 20)
  pure $ key <> " -> " <> value

p' :: Parser ByteString
p' = do
  void newline
  space
  BL.pack <$> some (printChar <|> char 20)

toByteStringP :: Parser Word8 -> Parser ByteString
toByteStringP w8P = BL.pack . (: []) <$> w8P

parseFileTest :: (Show a) => Parser a -> FilePath -> IO ()
printChars :: Parser ByteString
printChars = do
  void newline
  BL.pack . (: []) <$> printChar
parseFileTest parser path = do
  file <- BL.readFile path
  parseTest parser file
