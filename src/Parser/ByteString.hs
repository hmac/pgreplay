{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.ByteString where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as C8 (pack)
import           Data.UnixTime                    (UnixTime, parseUnixTime)
import           Prelude                          hiding (log, take)

data Log = Log { time     :: UnixTime
               , user     :: ByteString
               , database :: ByteString
               , session  :: ByteString
               , entry    :: Payload } deriving (Eq, Show)

data Payload = Detail { bindings :: [ByteString] }
              | Stmt ByteString
              | Duration
              | ParseStmt ByteString
              | BindStmt ByteString
              | ExecStmt ByteString
              | ConnRecv ByteString
              | ConnAuth ByteString
              | ConnDisc ByteString
              deriving (Eq, Show)

-- Parses a (possible multiline) log string, returning a single ByteString with no
-- newlines or tabs
parseLogStr :: Parser ByteString
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
  BS.concat <$> multi []

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
  pure $ parseUnixTime pgTimeFormat input

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
  where
    binding :: Parser ByteString
    binding = do
          char '$'
          _bindingIdx <- digit
          string " = "
          val <- quote '\'' $ many' bindChar
          pure $ C8.pack val
    bindChar = (string "''" >> pure '\'') <|> notChar '\''

entryWithDuration :: Parser Payload
entryWithDuration = do
  takeTill (== '.')
  char '.'
  take 3
  string " ms"
  choice [parseStmt, bindStmt, pure Duration]

parseStmt :: Parser Payload
parseStmt = string "  parse <unnamed>: " >> ParseStmt <$> unnamedStmt

bindStmt :: Parser Payload
bindStmt = string "  bind <unnamed>: " >> BindStmt <$> unnamedStmt

executeStmt :: Parser Payload
executeStmt = ExecStmt <$> unnamedStmt

unnamedStmt :: Parser ByteString
unnamedStmt = BS.concat <$> restOfLine `sepBy1` string "\n\t"


statementEntry :: Parser Payload
statementEntry = Stmt <$> restOfLine

connReceived :: Parser Payload
connReceived = ConnRecv <$> restOfLine

connAuthorized :: Parser Payload
connAuthorized = ConnAuth <$> restOfLine

connDisconnected :: Parser Payload
connDisconnected = ConnDisc <$> restOfLine

-- Parses all characters on a single line, delineated by \n
restOfLine :: Parser ByteString
restOfLine = takeTill (== '\n')

quote :: Char -> Parser a -> Parser a
quote c = between (char c) (char c)

between :: Parser a -> Parser b -> Parser c -> Parser c
between l r p = l >> p <* r

tab :: Parser Char
tab = char '\t'
