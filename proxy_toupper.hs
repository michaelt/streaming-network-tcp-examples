import Pipes.Network.TCP
import Control.Concurrent.Async 
import Pipes
import Control.Applicative


main = serve (Host "127.0.0.1") "4002" $ \(client, _) ->
       connect "127.0.0.1" "4000"      $ \(server, _) -> 
        do let act1 =  runEffect $ fromSocket server 1000 >-> toSocket client
               act2 =  runEffect $ fromSocket client 1000 >-> toSocket server
           runConcurrently (Concurrently act1 *> Concurrently act2) 




-- ------------------------------------------------------------------
-- {-# LANGUAGE OverloadedStrings #-}
-- import           Conduit
-- import           Control.Concurrent.Async (concurrently)
-- import           Control.Monad            (void)
-- import           Data.Conduit.Network

-- main :: IO ()
-- main =
--     runTCPServer (serverSettings 4002 "*") $ \client ->
--     runTCPClient (clientSettings 4000 "localhost") $ \server -> void $ concurrently
--         (appSource server $$ appSink client)
--         (appSource client $$ appSink server)

