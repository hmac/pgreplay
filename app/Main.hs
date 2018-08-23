import           System.Environment       (getArgs)

import           Conduit
import           Data.Conduit.Attoparsec
import qualified Data.Conduit.Combinators as C

import           Parser.ByteString        (Log (..), Payload (..), parseLog)

import           Data.Monoid              (Sum (..))

main :: IO ()
main = do
  [filename] <- getArgs
  stats <- runConduitRes
            $ C.sourceFile filename
           .| conduitParser Parser.ByteString.parseLog
           .| mapC (\(_, l) -> countLogTypes l)
           -- .| filterC isJust
           .| foldC
  let (statements, conns, discs) = stats
  putStrLn $ "Number of statements: " ++ show statements
  putStrLn $ "Number of connections: " ++ show conns
  putStrLn $ "Number of disconnections: " ++ show discs


countLogTypes :: Parser.ByteString.Log -> (Sum Int, Sum Int, Sum Int)
countLogTypes l = case entry l of
                    Stmt _     -> (Sum 1, Sum 0, Sum 0)
                    ConnRecv _ -> (Sum 0, Sum 1, Sum 0)
                    ConnDisc _ -> (Sum 0, Sum 0, Sum 1)
                    _          -> (Sum 0, Sum 0, Sum 0)
