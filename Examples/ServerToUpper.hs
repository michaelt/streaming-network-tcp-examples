module Examples.ServerToUpper (
-- * In which we define a server that send back strings upper-cased
-- $example

-- * Program
-- $program

-- ** Conduit version
-- $conduit
  main ) where

import Pipes
import Pipes.Network.TCP
import qualified Pipes.ByteString as Bytes

import Data.Word8 (toUpper)

main :: IO ()
main = do putStrLn "Opening upper-casing service on 4000"
          serve (Host "127.0.0.1") "4000" $ \(client,_) -> 
           runEffect $ fromSocket client 4096
                       >-> Bytes.map toUpper
                       >-> toSocket client   


{- $example 
This program sets up a service on 4000. We can start it thus:
                       
> terminal1$ pipes-network-tcp-examples ServerToUpper
> Opening upper-casing service on 4000
                       
and send it stuff with telnet:

> terminal2$ telnet localhost 4000
> Trying 127.0.0.1...
> Connected to localhost.
> Escape character is '^]'.
> hello
> HELLO

Or we can contact it with dedicated Haskell client like @Examples.ClientToUpper@ :

> terminal3$ pipes-network-tcp-examples ClientToUpper
> hello
> HELLO

                       
-}
                       
{- $program
                       
> import Pipes
> import Pipes.Network.TCP
> import qualified Pipes.ByteString as Bytes
> 
> import Data.Word8 (toUpper)
> 
> main :: IO ()
> main = do putStrLn "Opening upper-casing service on 4000"
>           serve (Host "127.0.0.1") "4000" $ \(client,_) -> 
>            runEffect $ fromSocket client 4096
>                        >-> Bytes.map toUpper
>                        >-> toSocket client   

-}

{- $conduit

And in the conduit version:

> import           Conduit
> import           Data.Conduit.Network
> import           Data.Word8           (toUpper)
> 
> main :: IO ()
> main = runTCPServer (serverSettings 4000 "*") $ \appData ->
>     appSource appData $$ omapCE toUpper =$ appSink appData
   
-}