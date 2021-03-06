{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.Text where

import           Control.Applicative       ((<|>))
import           Data.Attoparsec.Text.Lazy
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as C8 (pack)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8)
import           Data.UnixTime             (UnixTime, parseUnixTime)
import           Prelude                   hiding (log, take)

data Log = Log { time     :: UnixTime
               , user     :: T.Text
               , database :: T.Text
               , session  :: T.Text
               , entry    :: Payload } deriving (Eq, Show)

data Payload = Detail { bindings :: [T.Text] }
              | Stmt T.Text
              | Duration Double
              | ParseStmt T.Text
              | BindStmt T.Text
              | ExecStmt T.Text
              | ConnRecv T.Text
              | ConnAuth T.Text
              | ConnDisc T.Text
              deriving (Eq, Show)

-- Parses a (possible multiline) log string, returning a single Text with no
-- newlines or tabs
parseLogStr :: Parser T.Text
parseLogStr = do
  let multi ls = do
        line <- takeWhile1 (/= '\n')
        char '\n'
        c <- peekChar
        case c of
          Just '\t' -> do
            char '\t'
            multi (ls ++ [line])
          _         -> pure (ls ++ [line])
  T.unlines <$> multi []

-- A synonym for log that conflicts less
parseLog :: Parser Log
parseLog = log

-- Parses a single log
-- e.g. 2018-05-03 10:26:28.099 GMT|gc_paysvc_live|gc_paysvc_live|5aead9c5.68b7|DETAIL:...
log :: Parser Log
log = do
  time <- timestamp
  char '|'
  user <- takeTill (== '|')
  char '|'
  database <- takeTill (== '|')
  char '|'
  session <- takeTill (== '|')
  char '|'
  entry <- logEntry <|> detailEntry
  endOfLine
  pure Log { time, user, database, session, entry }

-- parses 2001-01-01 12:33:44.123 GMT
timestamp :: Parser UnixTime
timestamp = do
  input <- take 27
  pure $ parseUnixTime pgTimeFormat (encodeUtf8 input)

pgTimeFormat :: ByteString
pgTimeFormat = C8.pack "%Y-%m-%d %H:%M:%S%Q %Z"

logEntry :: Parser Payload
logEntry = do
  string "LOG:  "
  choice
    [ string "duration: " >> entryWithDuration
    , string "statement: " >> statementEntry
    , string "execute <unnamed>: " >> executeStmt
    , string "connection received: " >> connReceived
    , string "connection authorized: " >> connAuthorized
    , string "disconnection: " >> connDisconnected
    ]

detailEntry :: Parser Payload
detailEntry = do
  string "DETAIL:  parameters: "
  bs <- sepBy binding (string ", ")
  pure Detail { bindings = bs }
  where binding = do
          char '$'
          _bindingIdx <- digit
          string " = "
          val <- quote '\'' $ many' bindChar
          pure $ T.pack val
        bindChar = (string "''" >> pure '\'') <|> notChar '\''

entryWithDuration :: Parser Payload
entryWithDuration = do
  d <- double <* string " ms"
  choice [parseStmt, bindStmt, durationStmt d]

parseStmt :: Parser Payload
parseStmt = string "  parse <unnamed>: " >> ParseStmt <$> unnamedStmt

bindStmt :: Parser Payload
bindStmt = string "  bind <unnamed>: " >> BindStmt <$> unnamedStmt

executeStmt :: Parser Payload
executeStmt = ExecStmt <$> unnamedStmt

unnamedStmt :: Parser T.Text
unnamedStmt = T.unlines <$> restOfLine `sepBy1` (endOfLine >> tab)

durationStmt :: Double -> Parser Payload
durationStmt d = pure $ Duration d

statementEntry :: Parser Payload
statementEntry = Stmt <$> restOfLine

connReceived :: Parser Payload
connReceived = ConnRecv <$> restOfLine

connAuthorized :: Parser Payload
connAuthorized = ConnAuth <$> restOfLine

connDisconnected :: Parser Payload
connDisconnected = ConnDisc <$> restOfLine

-- Parses all characters on a single line, delineated by \n
restOfLine :: Parser T.Text
restOfLine = takeTill (== '\n')

quote :: Char -> Parser a -> Parser a
quote c = between (char c) (char c)

between :: Parser a -> Parser b -> Parser c -> Parser c
between l r p = l >> p <* r

tab :: Parser Char
tab = char '\t'
