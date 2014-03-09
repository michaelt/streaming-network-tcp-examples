{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE NoMonomorphismRestriction#-}

import Pipes.Network.TCP
import qualified Data.ByteString as B
import Data.ByteString.Char8 (ByteString)
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import Control.Concurrent.Async 
import Data.Word8 (toUpper, _cr)
import Pipes
import Lens.Family
import Data.Monoid
import Control.Monad
import Control.Applicative
import Pipes.Parse
import Lens.Family.State.Strict
import Pipes.Core

import Control.Monad (join)
import Lens.Family ((^.))
import Lens.Family.Unchecked (iso)
import Lens.Family2 (Lens')
import Prelude hiding (break, splitAt)


creds :: [(ByteString, ByteString)]
creds =
    [ ("spaceballs", "12345")
    , ("","")
    ]


checkAuth :: Producer ByteString IO r 
          -> Producer ByteString IO (Producer ByteString IO r)
checkAuth p = do
    yield "Username: "
    (username,p1) <- lift $ runStateT (zoom (line' . PB.splitAt 80) drawAll) p
    lift $ print username -- ["spaceballs","\r\n"]
    yield "Password: "
    (password,p2) <- lift $ runStateT (zoom (line' . PB.splitAt 80) drawAll) p1
    lift $ print password -- ["12345","\r\n"]
    let username' = B.filter (`notElem` [10,_cr]) (B.concat username)
    let password' = B.filter (`notElem` [10,_cr])  (B.concat password)
    lift $ print username' -- "spaceballs"
    lift $ print password' -- "12345"
    if (username', password') `elem` creds
           then do yield "Successfully authenticated.\n"
                   return p2
           else do yield "Invalid username/password.\n"
                   error "Invalid authentication, please log somewhere..."
                   return p2
 where
   line'
       :: Monad m
       => Lens' (Producer ByteString m r)
                (Producer ByteString m (Producer ByteString m r))
   line' = iso to join
     where
       to p = do
           p' <- p ^. PB.break  (== _cr)
           p' ^. PB.splitAt 2
   
main = serve (Host "127.0.0.1") "4003" $ \(client, _) -> 
   do let authorization = checkAuth (fromSocket client 1000) >-> toSocket client 
      from_client <- runEffect authorization
      connect  "127.0.0.1" "4000"  $ \(server,_) ->
        do let forward = runEffect $ from_client >-> toSocket server
               back    = runEffect $ fromSocket server 1000 >-> toSocket client
           concurrently forward back
           return ()
                                       
                                       