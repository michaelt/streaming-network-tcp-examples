{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE RankNTypes#-}
module Examples.ProxyAuth (main) where
import Pipes
import Pipes.Parse
import qualified Pipes.ByteString as PB
import Pipes.Network.TCP
import Control.Concurrent.Async
import qualified Data.ByteString as B
import Data.ByteString (ByteString) 
import Data.Word8 (_cr)
import Control.Monad
import Lens.Family.State.Strict
import Lens.Family 
import Lens.Family.Unchecked (iso)
import Lens.Family2 


creds :: [(ByteString, ByteString)]
creds = [ ("spaceballs", "12345") ]
  

shortLineInput :: Monad m => Int -> Parser ByteString m ByteString
shortLineInput n = do bss <- zoom (line' . PB.splitAt n) drawAll 
                      return $ B.filter (/= _cr) (B.concat bss)

checkAuth :: MonadIO m 
          => Producer ByteString m r 
          -> Producer ByteString m (Producer ByteString m r)
checkAuth p = do
    yield "Username: "
    (username,p1) <- lift $ runStateT (shortLineInput 80) p
    yield "Password: "
    (password,p2) <- lift $ runStateT (shortLineInput 80) p1
    if (username, password) `elem` creds
       then yield "Successfully authenticated.\n"
       else do yield "Invalid username/password.\n"
               error "Invalid authentication, please log somewhere..."
    return p2 -- when using `error` 

main :: IO ()
main = serve (Host "127.0.0.1") "4003" $ \(client, _) -> 
   do let authorization = checkAuth (fromSocket client 4096) >-> toSocket client 
      from_client <- runEffect authorization
      connect  "127.0.0.1" "4000"  $ \(server,_) ->
        do let pipe_forward = from_client            >-> toSocket server
               pipe_back    = fromSocket server 4096 >-> toSocket client
           concurrently (runEffect pipe_forward) (runEffect pipe_back)
           return ()

                      
line' :: Monad m => Lens' (Producer ByteString m r) 
                          (Producer ByteString m (Producer ByteString m r))
line' = iso to join where
  to p = do p' <- p ^. PB.line
            return (PB.drop 1 p')


-- ---------------------------------------------------------------------
-- creds :: [(ByteString, ByteString)]
-- creds =
--     [ ("spaceballs", "12345")
--     ]
-- 
-- checkAuth :: Conduit ByteString IO ByteString
-- checkAuth = do
--     yield "Username: "
--     username <- lineAsciiC $ takeCE 80 =$= filterCE (/= _cr) =$= foldC
--     yield "Password: "
--     password <- lineAsciiC $ takeCE 80 =$= filterCE (/= _cr) =$= foldC
--     if ((username, password) `elem` creds)
--         then do
--             yield "Successfully authenticated.\n"
--         else do
--             yield "Invalid username/password.\n"
--             error "Invalid authentication, please log somewhere..."
-- 
-- main :: IO ()
-- main =
--     runTCPServer (serverSettings 4003 "*") $ \client -> do
--         (fromClient, ()) <- appSource client $$+ checkAuth =$ appSink client
--         runTCPClient (clientSettings 4000 "localhost") $ \server ->
--             void $ concurrently
--                 (appSource server $$ appSink client)
--                 (fromClient $$+- appSink server)
