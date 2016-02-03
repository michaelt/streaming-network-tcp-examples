{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE RankNTypes#-}
module Examples.ProxyAuth (main) where

import qualified Data.ByteString.Streaming as Q
import Streaming.Network.TCP
import qualified Data.ByteString as B
import Data.ByteString (ByteString) 
import Data.Word8 (_cr)
import Streaming
import Control.Concurrent.Async 

creds :: [(ByteString, ByteString)]
creds = [ ("spaceballs", "12345") ]

checkAuth :: MonadIO m 
          => Q.ByteString m r 
          -> Q.ByteString m (Q.ByteString m r)
checkAuth p = do 
  Q.chunk "Username: "
  (username,p1) <- lift $ shortLineInput 80 p
  Q.chunk "Password: "
  (password,p2) <- lift $ shortLineInput 80 p1
  if (username, password) `elem` creds
       then Q.chunk "Successfully authenticated.\n"
       else do Q.chunk "Invalid username/password.\n"
               error "Invalid authentication, please log somewhere..."
  return p2 -- when using `error`

main :: IO ()
main = serve (Host "127.0.0.1") "4003" process  where
  process (client, _) =
   do from_client <- toSocket client (checkAuth (fromSocket client 4096))
      connect  "127.0.0.1" "4000"  $ \(server,_) ->
        do let pipe_forward = toSocket server from_client 
               pipe_back    = toSocket client (fromSocket server 4096) 
           concurrently pipe_forward pipe_back
           return ()


  
shortLineInput n bs = do
   (bs:>rest) <- Q.toStrict $ Q.break (==10) $ Q.splitAt n bs
   return $ (B.filter (/= _cr) bs, Q.drop 1 $ rest >>= id)
--   where
--   -- Bytes.line doesn't drop the following newline
--   line' = iso to (>>= id)
--   to p = do p' <- p ^. PB.line
--             return (PB.drop 1 p')

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
