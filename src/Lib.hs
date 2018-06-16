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

data Log = Log { time     :: UTCTime
               , user     :: T.Text
               , database :: T.Text
               , session  :: T.Text
               , entry    :: Payload } deriving (Eq, Show)

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
  pure Log { time = time, user = user, database = database, session = session, entry = entry }

-- parses 2001-01-01 12:33:44.123 GMT
timestamp :: Parser UTCTime
timestamp = do
  input <- T.unpack <$> take 27
  parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q %Z" input

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
  d <- msDuration
  choice [parseStmt, bindStmt, durationStmt d]

parseStmt :: Parser Payload
parseStmt = ParseStmt <$> unnamedStmt

bindStmt :: Parser Payload
bindStmt = BindStmt <$> unnamedStmt

executeStmt :: Parser Payload
executeStmt = ExecStmt <$> unnamedStmt

unnamedStmt :: Parser T.Text
unnamedStmt = T.unlines <$> restOfLine `sepBy1` (endOfLine >> tab)

durationStmt :: Float -> Parser Payload
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
