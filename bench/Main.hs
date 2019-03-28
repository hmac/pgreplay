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

  let statementLogT = T.pack statementLog
      durationLogT = T.pack durationLog
      parseStmtLogT = T.pack parseStmtLog
      parseStmtMultiLogT = T.pack parseStmtMultiLog
      bindStmtLogT = T.pack bindStmtLog
      execStmtLogT = T.pack execStmtLog
      connRecvLogT = T.pack connRecvLog
      connAuthLogT = T.pack connAuthLog
      connDiscLogT = T.pack connDiscLog
      detailLogT = T.pack detailLog

  defaultMain [
      bgroup "100 lines" [ bench "text" $ nfIO (parseLines smallT 0)
                         , bench "bytestring" $ nfIO (parseLinesBS smallBS 0)
                         ]
    , bgroup "1000 lines" [ bench "text" $ nfIO (parseLines mediumT 0)
                         , bench "bytestring" $ nfIO (parseLinesBS mediumBS 0)
                         ]
    , bgroup "10000 lines" [ bench "text" $ nfIO (parseLines largeT 0)
                         , bench "bytestring" $ nfIO (parseLinesBS largeBS 0)
                         ]
    , bgroup "micro/statement" [ bench "bytestring" $ nfIO (parseLineBS statementLog')
                               , bench "text" $ nfIO (parseLine statementLogT)
                               ]
    , bgroup "micro/duration" [ bench "bytestring" $ nfIO (parseLineBS durationLog')
                              , bench "text" $ nfIO (parseLine durationLogT)
                              ]
    , bgroup "micro/parse" [ bench "bytestring" $ nfIO (parseLineBS parseStmtLog')
                           , bench "text" $ nfIO (parseLine parseStmtLogT)
                           ]
    , bgroup "micro/parsemulti" [ bench "bytestring" $ nfIO (parseLineBS parseStmtMultiLog')
                                , bench "text" $ nfIO (parseLine parseStmtMultiLogT)
                                ]
    , bgroup "micro/bind" [ bench "bytestring" $ nfIO (parseLineBS bindStmtLog')
                          , bench "text" $ nfIO (parseLine bindStmtLogT)
                          ]
    , bgroup "micro/detail" [ bench "bytestring" $ nfIO (parseLineBS detailLog')
                            , bench "text" $ nfIO (parseLine detailLogT)
                            ]
    , bgroup "micro/execute" [ bench "bytestring" $ nfIO (parseLineBS execStmtLog')
                             , bench "text" $ nfIO (parseLine execStmtLogT)
                             ]
    , bgroup "micro/connrecv" [ bench "bytestring" $ nfIO (parseLineBS connRecvLog')
                              , bench "text" $ nfIO (parseLine connRecvLogT)
                              ]
    , bgroup "micro/connauth" [ bench "bytestring" $ nfIO (parseLineBS connAuthLog')
                              , bench "text" $ nfIO (parseLine connAuthLogT)
                              ]
    , bgroup "micro/conndisc" [ bench "bytestring" $ nfIO (parseLineBS connDiscLog')
                              , bench "text" $ nfIO (parseLine connDiscLogT)
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

parseLine :: T.Text -> IO ()
parseLine input =
  case AT.parse parseLog input of
    AT.Fail _ ctxs err -> error $ err ++ " " ++ show ctxs
    AT.Done rest _ ->
      if T.null rest
         then pure ()
         else error $ "input left: " ++ show rest

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
