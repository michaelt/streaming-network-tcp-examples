module Examples.ClientToUpper (main) where

import Pipes
import qualified Pipes.ByteString as PB
import Pipes.Network.TCP

import Control.Concurrent.Async (concurrently)

main = connect "127.0.0.1" "4000" $ \(connectionSocket,_) -> do
  let act1 = runEffect $ PB.stdin >-> toSocket connectionSocket
      act2 = runEffect $ fromSocket connectionSocket 4096 >-> PB.stdout
  concurrently act1 act2 
  return ()





-- --------------------------------------------------------------
--
-- {-# LANGUAGE OverloadedStrings #-}
-- import           Conduit
-- import           Control.Concurrent.Async (concurrently)
-- import           Control.Monad            (void)
-- import           Data.Conduit.Network
-- main =
--     runTCPClient (clientSettings 4000 "localhost") $ \server ->
--         void $ concurrently
--             (stdinC $$ appSink server)
--             (appSource server $$ stdoutC)

