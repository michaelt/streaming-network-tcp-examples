import Pipes.Network.TCP
import qualified Data.ByteString as B
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import           Control.Concurrent.Async (concurrently)
import Data.Word8 (toUpper)
import Pipes
import Lens.Family
import Data.Monoid
import Control.Monad

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

