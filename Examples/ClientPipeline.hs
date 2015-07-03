module Examples.ClientPipeline (main) where

import Pipes
import qualified Pipes.ByteString as PB
import Pipes.Network.TCP

import Control.Concurrent.Async 
import Control.Applicative

main = connect "127.0.0.1" "4000" $ \(socket1,_) ->
       connect "127.0.0.1" "4001" $ \(socket2,_) ->
        do let act1 = runEffect $ PB.stdin >-> toSocket socket1
               act2 = runEffect $ fromSocket socket1 4096 >-> toSocket socket2
               act3 = runEffect $ fromSocket socket2 4096 >-> PB.stdout
           runConcurrently $ Concurrently act1 *>
                             Concurrently act2 *>
                             Concurrently act3



-- --------------------------------------------------------------
-- 
-- {-# LANGUAGE OverloadedStrings #-}
-- import           Conduit
-- import           Control.Applicative      ((*>))
-- import           Control.Concurrent.Async (Concurrently (..))
-- import           Data.Conduit.Network
--
-- main =
--    runTCPClient (clientSettings 4000 "localhost") $ \server1 ->
--    runTCPClient (clientSettings 4001 "localhost") $ \server2 ->
--        runConcurrently $
--            Concurrently (stdinC $$ appSink server1) *>
--            Concurrently (appSource server1 $$ appSink server2) *>
--            Concurrently (appSource server2 $$ stdoutC)