import           Codec.Compression.GZip    (decompress)
import           Data.Attoparsec.Text.Lazy
import qualified Data.ByteString.Lazy      as BS (readFile)
import           Data.List                 (isSuffixOf)
import qualified Data.Text.Lazy            as T
import           Data.Text.Lazy.Encoding   (decodeUtf8)
import qualified Data.Text.Lazy.IO         as T (readFile)
import           Prelude                   hiding (readFile)
import           System.Environment        (getArgs)
import           Text.Pretty.Simple

import           Lib                       (parseLog)

main :: IO ()
main = do
  [filename] <- getArgs
  let readFile = if ".gz" `isSuffixOf` filename then readGzipFile else T.readFile
  file <- readFile filename
  go parseLog file 0
    where
      go parser input len = case parse parser input of
              Fail rest ctxs err -> error err
              Done rest res      -> do
                if T.null rest
                   then putStrLn $ "Parsed " ++ show len ++ " log lines."
                   else do
                     pPrint res
                     go parser rest (len+1)

readGzipFile :: String -> IO T.Text
readGzipFile filename = decodeUtf8 . decompress <$> BS.readFile filename

