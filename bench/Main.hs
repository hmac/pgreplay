module Main where

import           Criterion.Main
import           Data.Attoparsec.Text.Lazy (Result (..), parse)
import qualified Data.Text.Lazy            as T
import qualified Data.Text.Lazy.IO         as T (readFile)
import           Lib                       (parseLog)

main :: IO ()
main = do
  small <- smallSample
  medium <- mediumSample
  large <- largeSample
  defaultMain [
    bgroup "parse" [ bench "100 lines" $ nfIO (parseLines small 0)
                   , bench "1000 lines" $ nfIO (parseLines medium 0)
                   , bench "10000 lines" $ nfIO (parseLines large 0)
                   ]
    ]

parseLines :: T.Text -> Int -> IO Int
parseLines input len =
  case parse parseLog input of
    Fail _ _ err -> error err
    Done rest _ ->
      if T.null rest
         then pure len
        else parseLines rest (len+1)

smallSample :: IO T.Text
smallSample = T.readFile "small.log"

mediumSample :: IO T.Text
mediumSample = T.readFile "sample.log"

largeSample :: IO T.Text
largeSample = T.readFile "large.log"
