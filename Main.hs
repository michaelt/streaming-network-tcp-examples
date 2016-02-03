module Main where

import Streaming
import Streaming.Network.TCP
import qualified Streaming.Prelude as S
import qualified Data.ByteString.Streaming  as Q

import Control.Concurrent.Async
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Word8 (toUpper, _cr)
import Data.Function ((&))

import Options.Applicative
import Control.Applicative
import Control.Monad
import Data.Monoid


serverToUpper :: IO ()
serverToUpper = do
    putStrLn "Opening upper-casing service on 4000"
    serve (Host "127.0.0.1") "4000" $ \(client,_) -> 
      toSocket client $ Q.map toUpper $ fromSocket client 4096 
                                     

serverDoubler :: IO ()
serverDoubler = do 
  putStrLn "Double server available on 4001"
  serve (Host "127.0.0.1") "4001" $ \(connectionSocket, remoteAddr) -> 
    fromSocket connectionSocket 4096
          & Q.toChunks
          & S.map (B.concatMap (\x -> B.pack [x,x]))
          & Q.fromChunks
          & toSocket connectionSocket

clientToUpper :: IO ()
clientToUpper = connect "127.0.0.1" "4000" $ \(connectionSocket,_) -> do
  let act1 = toSocket connectionSocket Q.stdin  
      act2 = Q.stdout (fromSocket connectionSocket 4096) 
  concurrently act1 act2 
  return ()


clientPipeline :: IO ()
clientPipeline = do
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

proxyToUpper :: IO ()
proxyToUpper = 
  serve (Host "127.0.0.1") "4002" $ \(client, _) ->
    connect "127.0.0.1" "4000"    $ \(server, _) -> 
      do let act1 =  toSocket server (fromSocket client 4096)
             act2 =  toSocket client (fromSocket server 4096)
         concurrently act1 act2
         return ()


proxyAuth :: IO ()
proxyAuth = serve (Host "127.0.0.1") "4003" process  
  where
  process (client, _) =
    do from_client <- toSocket client (checkAuth (fromSocket client 4096))
       connect  "127.0.0.1" "4000"  $ \(server,_) ->
         do let pipe_forward = toSocket server from_client 
                pipe_back    = toSocket client (fromSocket server 4096) 
            concurrently pipe_forward pipe_back
            return ()

  checkAuth :: MonadIO m  => Q.ByteString m r  -> Q.ByteString m (Q.ByteString m r)
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
 
  shortLineInput n bs = do
     (bs:>rest) <- Q.toStrict $ Q.break (==10) $ Q.splitAt n bs
     return $ (B.filter (/= _cr) bs, Q.drop 1 $ rest >>= id) 
    
  creds :: [(ByteString, ByteString)]
  creds = [ ("spaceballs", "12345") ]



main :: IO ()
main = join $ execParser (info opts idm) where

  opts :: Parser (IO ())
  opts = helper <*> subparser stuff where 
     stuff = mconcat
      [ command "ClientPipeline" (info (pure clientPipeline) idm)
      , command "ClientToUpper"  (info (pure clientToUpper) idm)
      , command "ProxyAuth"      (info (pure proxyAuth) idm)
      , command "ProxyToUpper"   (info (pure proxyToUpper) idm)
      , command "ServerDouble"   (info (pure serverDoubler) idm)
      , command "ServerToUpper"  (info (pure serverToUpper) idm)
      ]
  


