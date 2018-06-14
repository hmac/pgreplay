{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Control.Applicative       ((<|>))
import           Data.Attoparsec.Text.Lazy
import           Data.Char                 (isDigit)
import qualified Data.Text                 as T
import           Data.Time.Clock           (UTCTime)
import           Data.Time.Format
import           Data.Void
import           Prelude                   hiding (log, take)

data Log = Log { time      :: UTCTime
               , database  :: T.Text
               , sessionId :: T.Text
               , entry     :: Payload } deriving (Eq, Show)

data Payload = Detail { bindings :: [T.Text] }
              | Stmt T.Text
              | Duration Float
              | ParseStmt T.Text
              | BindStmt T.Text
              | ExecStmt T.Text
              | ConnRecv T.Text
              | ConnAuth T.Text
              | ConnDisc T.Text
              deriving (Eq, Show)

-- A synonym for log that conflicts less
parseLog = log

-- Parses a single log
log :: Parser Log
log = do
  time <- timestamp
  char '|'
  database <- takeTill (== '|')
  char '|'
  _ <- takeTill (== '|')
  char '|'
  sessionId <- takeTill (== '|')
  char '|'
  entry <- logEntry <|> detailEntry
  endOfLine
  pure Log { time = time, database = database, sessionId = sessionId, entry = entry }

-- parses 2001-01-01 12:33:44.123 GMT
timestamp :: Parser UTCTime
timestamp = do
  input <- T.unpack <$> take 27
  parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q %Z" input

logEntry :: Parser Payload
logEntry = do
  string "LOG:  "
  choice [statementEntry
        , detailEntry
        , executeStmt
        , connReceived
        , connAuthorized
        , connDisconnected
        , entryWithDuration]

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
  d <- duration
  choice [parseStmt, bindStmt, durationStmt d]

duration :: Parser Float
duration = string "duration: " >> msDuration

parseStmt :: Parser Payload
parseStmt = do
  string "  parse <unnamed>: "
  ParseStmt <$> unnamedStmt

bindStmt :: Parser Payload
bindStmt = do
  string "  bind <unnamed>: "
  BindStmt <$> unnamedStmt

executeStmt :: Parser Payload
executeStmt = do
  string "execute <unnamed>: "
  ExecStmt <$> unnamedStmt

unnamedStmt :: Parser T.Text
unnamedStmt = T.unlines <$> textLine `sepBy1` (endOfLine >> tab)

durationStmt :: Float -> Parser Payload
durationStmt d = pure $ Duration d

statementEntry :: Parser Payload
statementEntry = string "statement: " >> Stmt <$> textLine

connReceived :: Parser Payload
connReceived = string "connection received: " >> ConnRecv <$> textLine

connAuthorized :: Parser Payload
connAuthorized = string "connection authorized: " >> ConnAuth <$> textLine

connDisconnected :: Parser Payload
connDisconnected = string "disconnection: " >> ConnDisc <$> textLine

-- Parses all characters on a single line, delineated by \n
textLine :: Parser T.Text
textLine = takeTill (== '\n')

msDuration :: Parser Float
msDuration = do
  secs <- read . T.unpack <$> takeWhile1 isDigit
  char '.'
  millis <- read . T.unpack <$> takeWhile1 isDigit
  string " ms"
  pure $ secs + millis / 1000

quote :: Char -> Parser a -> Parser a
quote c = between (char c) (char c)

between :: Parser a -> Parser b -> Parser c -> Parser c
between l r p = l >> p <* r

tab :: Parser Char
tab = char '\t'

sampleLog :: T.Text
sampleLog = T.pack "2018-05-03 10:26:28.099 GMT|gc_paysvc_live|gc_paysvc_live|5aead9c5.68b7|DETAIL:  parameters: $1 = '', $2 = '30', $3 = '2018-05-03 10:26:27.905086+00', $4 = '544195344', $5 = 'this is a quote: '''"
