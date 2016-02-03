module Examples.ServePipes (main) where
  
import Streaming.Network.TCP
import qualified Data.ByteString.Streaming as Q
import Data.Word8 (toUpper)

main :: IO ()
main = serve (Host "127.0.0.1") "4000" $ \(client,_) -> 
         toSocket client $ Q.map toUpper $ fromSocket client 4096
 





-- --------------------------------------------------------------
--
-- {-# LANGUAGE OverloadedStrings #-}
-- import           Conduit
-- import           Data.Conduit.Network
-- import           Data.Word8           (toUpper)
--
-- main :: IO ()
-- main = runTCPServer (serverSettings 4000 "*") $ \appData ->
--     appSource appData $$ omapCE toUpper =$ appSink appData