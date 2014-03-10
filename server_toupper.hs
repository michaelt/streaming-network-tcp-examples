import Pipes.Network.TCP
import qualified Data.ByteString as B
import qualified Pipes.Prelude as P
import Data.Word8 (toUpper)
import Pipes
main = serve (Host "127.0.0.1") "4000" $ \(client,_) -> 
       runEffect $ fromSocket client 4000 
                   >-> P.map (B.map toUpper) 
                   >-> toSocket client   





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