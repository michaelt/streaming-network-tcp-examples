module Examples.ProxyToUpper (main) where

import Pipes
import Pipes.Network.TCP
import Control.Concurrent.Async 
import Control.Monad

main :: IO ()
main = serve (Host "127.0.0.1") "4002" $ \(client, _) ->
       connect "127.0.0.1" "4000"      $ \(server, _) -> 
        do let act1 =  runEffect $ fromSocket server 4096 >-> toSocket client
               act2 =  runEffect $ fromSocket client 4096 >-> toSocket server
           void $ concurrently act1 act2




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

