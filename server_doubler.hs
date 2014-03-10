import Pipes.Network.TCP
import qualified Data.ByteString as B
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as Bytes
import Data.Word8 (toUpper)
import Pipes

main :: IO ()
main = serve (Host "127.0.0.1") "4001" $ \(connectionSocket, remoteAddr) -> 
           runEffect $ fromSocket connectionSocket 4000 
                        >-> Bytes.concatMap (\x -> B.pack [x,x])
                        >-> toSocket connectionSocket
                        



-- --------------------------------------------------------------
--
-- {-# LANGUAGE OverloadedStrings #-}
-- import           Conduit
-- import           Data.ByteString      (pack)
-- import           Data.Conduit.Network
-- main = runTCPServer (serverSettings 4001 "*") $ \appData ->
--     appSource appData
--         $$ concatMapCE (\w -> pack [w, w])
--         =$ appSink appData