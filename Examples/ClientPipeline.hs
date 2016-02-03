module Examples.ClientPipeline (
-- * In which we define a client that links remote doubling and uppercasing services
-- $example
-- * Program
-- $program
-- ** Conduit version
-- $conduit
   main
   ) where


import qualified Data.ByteString.Streaming as Q
import Streaming.Network.TCP

import Control.Concurrent.Async 
import Control.Applicative

main = do
  putStrLn "We will connect stdin to 4000 and 4001 in succession."
  putStrLn "Input will thus be uppercased and doubled char-by-char.\n"
  connect "127.0.0.1" "4000" $ \(socket1,_) ->
    connect "127.0.0.1" "4001" $ \(socket2,_) ->
      do let act1 = toSocket socket1 (Q.stdin)
             act2 = toSocket socket2 (fromSocket socket1 4096)
             act3 = Q.stdout (fromSocket socket2 4096)
         runConcurrently $ Concurrently act1 *>
                           Concurrently act2 *>
                           Concurrently act3

{- $example

If the uppercasing server in @Examples.ServerToUpper@ is running

> terminal1$ pipes-network-tcp-examples ServerToUpper
> Opening upper-casing service on 4000
     
and the text-doubling server is available:
                                 
> terminal2$ pipes-network-tcp-examples ServerDouble
> Double server available on 4001
                             
then we can interact with both at once using this program:       

> terminal3$ pipes-network-tcp-examples ClientPipeline
> We will connect stdin to 4000 and 4001 in succession.
> Input will thus be uppercased and doubled char-by-char.
> 
> double upper!
> DDOOUUBBLLEE  UUPPPPEERR!!
> 
> upper double!
> UUPPPPEERR  DDOOUUBBLLEE!!

-}
                           
{- $program                                         
> import Pipes
> import qualified Pipes.ByteString as PB
> import Pipes.Network.TCP
> 
> import Control.Concurrent.Async 
> import Control.Applicative
> 
> main = do
>   putStrLn "We will connect stdin to 4000 and 4001 in succession."
>   putStrLn "Input will thus be uppercased and doubled char-by-char.\n"
>   connect "127.0.0.1" "4000" $ \(socket1,_) ->
>     connect "127.0.0.1" "4001" $ \(socket2,_) ->
>       do let act1 = runEffect $ PB.stdin >-> toSocket socket1
>              act2 = runEffect $ fromSocket socket1 4096 >-> toSocket socket2
>              act3 = runEffect $ fromSocket socket2 4096 >-> PB.stdout
>          runConcurrently $ Concurrently act1 *>
>                            Concurrently act2 *>
>                            Concurrently act3
>                              
> 
-}
{- $conduit                                                       
And, in the conduit version: 
                             
> 
> import           Conduit
> import           Control.Applicative      ((*>))
> import           Control.Concurrent.Async (Concurrently (..))
> import           Data.Conduit.Network
> 
> main =
>    runTCPClient (clientSettings 4000 "localhost") $ \server1 ->
>    runTCPClient (clientSettings 4001 "localhost") $ \server2 ->
>        runConcurrently $
>            Concurrently (stdinC $$ appSink server1) *>
>            Concurrently (appSource server1 $$ appSink server2) *>
>            Concurrently (appSource server2 $$ stdoutC)

-}