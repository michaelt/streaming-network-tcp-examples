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


main = connect "127.0.0.1" "4000" $ \(sock1, remoteAddr1) ->
       connect "127.0.0.1" "4001" $ \(sock2, remoteAddr2) ->
        do let act1 = runEffect $ PB.stdin >-> toSocket sock1
               act2 = runEffect $ fromSocket sock1 4000 >-> toSocket sock2
               act3 = runEffect $ fromSocket sock2 4000 >-> PB.stdout
           runConcurrently $ Concurrently act1 *>
                             Concurrently act2 *>
                             Concurrently act3
