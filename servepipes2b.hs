import Pipes.Network.TCP
import qualified Data.ByteString as B
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import           Control.Concurrent.Async 
import Data.Word8 (toUpper)
import Pipes
import Lens.Family
import Data.Monoid
import Control.Monad
import Control.Applicative


main = serve (Host "127.0.0.1") "4002" $ \(client, remoteAddr1) ->
       connect "127.0.0.1" "4001" $ \(server, remoteAddr2) ->
        do let act1 =  fromSocket server 1000 >-> toSocket client
               act2 =  fromSocket client 1000 >-> toSocket server
           void $ concurrently (runEffect act1) (runEffect act2)
                