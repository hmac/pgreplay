module Main where

import           Criterion.Main
import qualified Data.Attoparsec.ByteString.Lazy as AB (Result (..), parse)
import qualified Data.Attoparsec.Text.Lazy       as AT (Result (..), parse)
import qualified Data.ByteString.Lazy            as BS
import qualified Data.ByteString.Lazy.Char8      as C8 (pack)
import qualified Data.Text.Lazy                  as T
import qualified Data.Text.Lazy.IO               as T (readFile)
import qualified Parser.ByteString               (parseLog)
import           Parser.Text                     (parseLog)

main :: IO ()
main = do
  smallT <- T.readFile "samples/small.log"
  mediumT <- T.readFile "samples/medium.log"
  largeT <- T.readFile "samples/large.log"
  smallBS <- BS.readFile "samples/small.log"
  mediumBS <- BS.readFile "samples/medium.log"
  largeBS <- BS.readFile "samples/large.log"

  let statementLog' = C8.pack statementLog
      durationLog' = C8.pack durationLog
      parseStmtLog' = C8.pack parseStmtLog
      parseStmtMultiLog' = C8.pack parseStmtMultiLog
      bindStmtLog' = C8.pack bindStmtLog
      execStmtLog' = C8.pack execStmtLog
      connRecvLog' = C8.pack connRecvLog
      connAuthLog' = C8.pack connAuthLog
      connDiscLog' = C8.pack connDiscLog
      detailLog' = C8.pack detailLog

  defaultMain [
      bgroup "text" [ bench "100 lines" $ nfIO (parseLines smallT 0)
                    , bench "1000 lines" $ nfIO (parseLines mediumT 0)
                    , bench "10000 lines" $ nfIO (parseLines largeT 0)
                    ]
    , bgroup "bytestring" [ bench "100 lines" $ nfIO (parseLinesBS smallBS 0)
                          , bench "1000 lines" $ nfIO (parseLinesBS mediumBS 0)
                          , bench "10000 lines" $ nfIO (parseLinesBS largeBS 0)
                          ]
    , bgroup "bytestring/micro" [bench "statement" $ nfIO (parseLineBS statementLog')
                               , bench "duration" $ nfIO (parseLineBS durationLog')
                               , bench "parse" $ nfIO (parseLineBS parseStmtLog')
                               , bench "parsemulti" $ nfIO (parseLineBS parseStmtMultiLog')
                               , bench "bind" $ nfIO (parseLineBS bindStmtLog')
                               , bench "detail" $ nfIO (parseLineBS detailLog')
                               , bench "execute" $ nfIO (parseLineBS execStmtLog')
                               , bench "connrecv" $ nfIO (parseLineBS connRecvLog')
                               , bench "connauth" $ nfIO (parseLineBS connAuthLog')
                               , bench "conndisc" $ nfIO (parseLineBS connDiscLog')
                               ]
    ]

parseLines :: T.Text -> Int -> IO Int
parseLines input len =
  case AT.parse parseLog input of
    AT.Fail _ _ err -> error err
    AT.Done rest _ ->
      if T.null rest
        then pure len
        else parseLines rest (len + 1)

parseLinesBS :: BS.ByteString -> Int -> IO Int
parseLinesBS input len =
  case AB.parse Parser.ByteString.parseLog input of
    AB.Fail _ _ err -> error err
    AB.Done rest _ ->
      if BS.null rest
        then pure len
        else parseLinesBS rest (len + 1)


parseLineBS :: BS.ByteString -> IO ()
parseLineBS input =
  case AB.parse Parser.ByteString.parseLog input of
    AB.Fail _ ctxs err -> error $ err ++ " " ++ show ctxs
    AB.Done rest _ ->
      if BS.null rest
         then pure ()
         else error $ "input left: " ++ show rest

statementLog = "2018-05-03 10:26:28.083 GMT|gc_paysvc_live|gc_paysvc_live|5aeae3ca.38fd|LOG:  statement: SET application_name='unicorn_rails worker[1] -c config/unicorn.rb';\n"
durationLog = "2018-05-03 10:26:28.083 GMT|gc_paysvc_live|gc_paysvc_live|5aeae3ca.38fd|LOG:  duration: 0.037 ms\n"
parseStmtLog = "2018-05-03 10:26:28.083 GMT|gc_paysvc_live|gc_paysvc_live|5aeae241.7d3b|LOG:  duration: 0.112 ms  parse <unnamed>: SELECT  \"pending_resources\".* FROM \"pending_resources\" WHERE \"pending_resources\".\"id\" = 'PD0002JJARBKNS' LIMIT 1 /*application:PaymentsService,request_id:b6f5971e-844a-4399-b494-b871a410553e,handler:Routes::Pay::Flow::Success*/\n"
bindStmtLog = "2018-05-03 10:26:28.083 GMT|gc_paysvc_live|gc_paysvc_live|5aeae241.7d3b|LOG:  duration: 0.170 ms  bind <unnamed>: SELECT  \"pending_resources\".* FROM \"pending_resources\" WHERE \"pending_resources\".\"id\" = 'PD0002JJARBKNS' LIMIT 1 /*application:PaymentsService,request_id:b6f5971e-844a-4399-b494-b871a410553e,handler:Routes::Pay::Flow::Success*/\n"
execStmtLog = "2018-05-03 10:26:28.083 GMT|gc_paysvc_live|gc_paysvc_live|5aeae241.7d3b|LOG:  execute <unnamed>: SELECT  \"pending_resources\".* FROM \"pending_resources\" WHERE \"pending_resources\".\"id\" = 'PD0002JJARBKNS' LIMIT 1 /*application:PaymentsService,request_id:b6f5971e-844a-4399-b494-b871a410553e,handler:Routes::Pay::Flow::Success*/\n"
connRecvLog = "2018-05-03 10:26:28.091 GMT|[unknown]|[unknown]|5aeae3d4.3aef|LOG:  connection received: host=[local]\n"
connAuthLog = "2018-05-03 10:26:28.092 GMT|postgres|template1|5aeae3d4.3aef|LOG:  connection authorized: user=postgres database=template1\n"
connDiscLog = "2018-05-03 10:26:28.093 GMT|postgres|template1|5aeae3d4.3aef|LOG:  disconnection: session time: 0:00:00.002 user=postgres database=template1 host=[local]\n"
parseStmtMultiLog = "2018-05-03 10:26:28.099 GMT|gc_paysvc_live|gc_paysvc_live|5aead9c5.68b7|LOG:  duration: 0.096 ms  parse <unnamed>: \n\t      DELETE FROM que_jobs \n\t      WHERE queue    = $1::text \n\t      AND   priority = $2::smallint \n\t      AND   run_at   = $3::timestamptz \n\t      AND   job_id   = $4::bigint \n\t    \n"
parseStmtBind = "2018-05-03 10:26:28.099 GMT|gc_paysvc_live|gc_paysvc_live|5aead9c5.68b7|LOG:  duration: 0.096 ms  parse <unnamed>: \n\t      DELETE FROM que_jobs \n\t      WHERE queue    = $1::text \n\t      AND   priority = $2::smallint \n\t      AND   run_at   = $3::timestamptz \n\t      AND   job_id   = $4::bigint \n\t    \n"
detailLog = "2018-05-03 10:26:28.099 GMT|gc_paysvc_live|gc_paysvc_live|5aead9c5.68b7|DETAIL:  parameters: $1 = '', $2 = '30', $3 = '2018-05-03 10:26:27.905086+00', $4 = '544195344'\n"
