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

main = connect "127.0.0.1" "4000" $ \(connectionSocket, remoteAddr) -> do
  let act1 = runEffect $ PB.stdin >-> toSocket connectionSocket
      act2 = runEffect $ fromSocket connectionSocket 4000 >-> PB.stdout
  void $ concurrently act1 act2 

