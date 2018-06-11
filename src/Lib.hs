module Lib (logFile) where

import qualified Data.Text.Lazy   as T
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format
import           Prelude          hiding (log)
import           Text.Megaparsec

type Parser = Parsec Dec T.Text

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

-- Parses an entire log file
logFile :: Parser [Log]
logFile = log `sepEndBy1` newline <* eof

-- Parses a single log
log :: Parser Log
log = do
  time <- timestamp
  char '|'
  database <- T.pack <$> some (noneOf "|")
  char '|'
  _ <- T.pack <$> some (noneOf "|")
  char '|'
  sessionId <- T.pack <$> some (noneOf "|")
  char '|'
  entry <- logEntry <|> detailEntry
  pure Log { time = time, database = database, sessionId = sessionId, entry = entry }

-- parses 2001-01-01 12:33:44.123 GMT
timestamp :: Parser UTCTime
timestamp = label "timestamp" $ do
  input <- count 27 asciiChar
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
          _bindingIdx <- digitChar
          string " = "
          val <- quote '\'' $ many bindChar
          pure $ T.pack val
        bindChar = (string "''" >> pure '\'') <|> noneOf "'"

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
unnamedStmt = T.unlines <$> textLine `sepBy1` try (newline >> tab)

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
textLine = T.pack <$> many (noneOf "\n")

msDuration :: Parser Float
msDuration = label "duration_ms" $ do
  secs <- read <$> some digitChar
  char '.'
  millis <- read <$> some digitChar
  string " ms"
  pure $ secs + millis / 1000

quote :: Char -> Parser a -> Parser a
quote c = between (char c) (char c)

sampleLog :: T.Text
sampleLog = T.pack "2018-05-03 10:26:28.099 GMT|gc_paysvc_live|gc_paysvc_live|5aead9c5.68b7|DETAIL:  parameters: $1 = '', $2 = '30', $3 = '2018-05-03 10:26:27.905086+00', $4 = '544195344', $5 = 'this is a quote: '''"
