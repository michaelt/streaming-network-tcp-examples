module Examples.ClientToUpper (
-- * In which we define a client that contacts our remote uppercasing service
-- $example
-- * Program
-- $program
-- ** Conduit version
-- $conduit
   main
   ) where

import Pipes
import qualified Pipes.ByteString as PB
import Pipes.Network.TCP

import Control.Concurrent.Async (concurrently)

main = connect "127.0.0.1" "4000" $ \(connectionSocket,_) -> do
  let act1 = runEffect $ PB.stdin >-> toSocket connectionSocket
      act2 = runEffect $ fromSocket connectionSocket 4096 >-> PB.stdout
  concurrently act1 act2 
  return ()

{- $example

If we have our uppercasing service from @Examples.ServerToUpper@ running on 4000 
  
> terminal1$ pipes-network-tcp-examples ServerToUpper
> Opening upper-casing service on 4000

then we can telnet to it, of course, but here we have a dedicated client for talking to it:

> terminal2$ pipes-network-tcp-examples ClientToUpper
> el pueblo unido jamas sera vencido!
> EL PUEBLO UNIDO JAMAS SERA VENCIDO!
  
  
-}

{- $program
> import Pipes
> import qualified Pipes.ByteString as PB
> import Pipes.Network.TCP
> import Control.Concurrent.Async (concurrently)
> 
> main = connect "127.0.0.1" "4000" $ \(connectionSocket,_) -> do
>   let act1 = runEffect $ PB.stdin >-> toSocket connectionSocket
>       act2 = runEffect $ fromSocket connectionSocket 4096 >-> PB.stdout
>   concurrently act1 act2 
>   return ()
-}

{- $conduit
> import           Conduit
> import           Control.Concurrent.Async (concurrently)
> import           Control.Monad            (void)
> import           Data.Conduit.Network
> main =
>     runTCPClient (clientSettings 4000 "localhost") $ \server ->
>         void $ concurrently
>             (stdinC $$ appSink server)
>             (appSource server $$ stdoutC)
  
-}