import           Codec.Compression.GZip  (decompress)
import           Criterion
import qualified Data.ByteString.Lazy    as BS (readFile)
import           Data.List               (isSuffixOf)
import qualified Data.Text.Lazy          as T
import           Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.IO       as T (readFile)
import           Prelude                 hiding (readFile)
import           System.Environment      (getArgs)
import           Text.Megaparsec
import           Text.Pretty.Simple

import           Lib                     (logFile)

main :: IO ()
main = do
  [filename] <- getArgs
  let readFile = if ".gz" `isSuffixOf` filename then readGzipFile else readTextFile
  file <- readFile filename
  -- benchMain file
  case parse logFile "" file of
    Left e  -> error $ parseErrorPretty e
    Right t -> do
      -- pPrint t
      pPrint (head t)
      -- putStrLn $ "Parsed " ++ (show . length) t ++ " log items successfully."

benchMain :: T.Text -> IO ()
benchMain file = benchmark $ nfIO (run file)
  where
    run f = case parse logFile "" f of
                Left e  -> error $ parseErrorPretty e
                Right t -> putStr "."

readTextFile :: String -> IO T.Text
readTextFile = T.readFile

readGzipFile :: String -> IO T.Text
readGzipFile filename = (decodeUtf8 . decompress) <$> BS.readFile filename

