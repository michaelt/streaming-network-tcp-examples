import Pipes.Network.TCP
import qualified Data.ByteString as B
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import Control.Concurrent.Async 
import Data.Word8 (toUpper)
import Pipes
import Lens.Family
import Data.Monoid
import Control.Monad
import Control.Applicative


main = serve (Host "127.0.0.1") "4002" $ \(client, _) ->
       connect "127.0.0.1" "4001" $ \(server1, _) ->
       connect "127.0.0.1" "4000" $ \(server2, _) -> 
        do let act1 =  runEffect $ fromSocket server2 1000 >-> toSocket client
               act2 =  runEffect $ fromSocket client 1000 >-> toSocket server1
               act3 =  runEffect $ fromSocket server1 1000 >-> toSocket server2
               c = Concurrently
           runConcurrently $ c act1 *> c act2 *> c act3