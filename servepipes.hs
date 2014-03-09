import Pipes.Network.TCP
import qualified Data.ByteString as B
import qualified Pipes.Prelude as P
import Data.Word8 (toUpper)
import Pipes
main = serve (Host "127.0.0.1") "4000" $ \(connectionSocket, remoteAddr) -> 
           runEffect $ fromSocket connectionSocket 4000 
                        >-> P.map (B.map toUpper)
                        >-> toSocket connectionSocket   