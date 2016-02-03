module Examples.ServerDouble ( 
-- * In which we define a server that the doubles the characters in incoming strings
-- $example
-- * Program
-- $program
-- ** Conduit version
-- $conduit
   main 
   ) where

import Streaming.Network.TCP
import qualified Streaming.Prelude as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Streaming as Q
import Data.Function ((&))

main :: IO ()
main = do putStrLn "Double server available on 4001"
          serve (Host "127.0.0.1") "4001" $ \(connectionSocket, remoteAddr) -> 
            fromSocket connectionSocket 4096
                  & Q.toChunks
                  & S.map (B.concatMap (\x -> B.pack [x,x]))
                  & Q.fromChunks
                  & toSocket connectionSocket
                        
{- $example

This program sets up a service on 4001. We can start it thus:
                       
> terminal1$ pipes-network-tcp-examples ServerDouble
> Double server available on 4001
                        
and send it stuff with telnet:

> terminal2$ telnet localhost 4001
> Trying 127.0.0.1...
> Connected to localhost.
> Escape character is '^]'.
> hello
> hheelllloo

Our dedicated client in @Examples.ClientPipeline@ will link this server to the uppercasing server.

-}

{- $program 
> import Pipes.Network.TCP
> import qualified Data.ByteString as B
> import qualified Pipes.ByteString as Bytes
> import Pipes                        
                        
> main :: IO ()
> main = do putStrLn "Double server available on 4001"
>           serve (Host "127.0.0.1") "4001" $ \(connectionSocket, remoteAddr) -> 
>             runEffect $ fromSocket connectionSocket 4096
>                         >-> Bytes.concatMap (\x -> B.pack [x,x])
>                         >-> toSocket connectionSocket

-}
                        
{- $conduit
                        
And, in the conduit version:                     

> import           Conduit
> import           Data.ByteString      (pack)
> import           Data.Conduit.Network
> main = runTCPServer (serverSettings 4001 "*") $ \appData ->
>     appSource appData
>         $$ concatMapCE (\w -> pack [w, w])
>         =$ appSink appData
>         =$ appSink appData
                        
-}