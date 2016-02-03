streaming-network-tcp-examples
==============================

These mechanically follow the pleasantly
transparent 'hello world'-ish examples of TCP connections in
http://www.yesodweb.com/blog/2014/03/network-conduit-async .

Apart from `Streaming.Network.TCP` and `async` we use the
`word8` library for humane word8 handling. 

The variants follow Michael S's text in this
order:

-   `serverToUpper`
    -   a server on 4000 that sends back input sent e.g. with telnet
        upper-cased or 'angry'
-   `serverDouble`
    -   a server on 4001 that sends back 
        input doubled, `Char8` by `Char8`
-   `clientToUpper`
    -   a client through which the user interacts
        directly to the "angry" server 
-   `clientPipeline`
    -   a client that sends material to the
        "angry" server and the doubling server and
        returns it to the user
-   `proxyToUpper`
    -   a proxy on 4002 that sends input to the
        angry server on 4000
-   `proxyAuth`
    -   a proxy on 4003 that asks for demands
        authorization before condescending to send
        user input to the angry server on 4000

The following remarks will require that eight
instances of a terminal all be opened in the main
directory of the repository; if you like you can
`cabal install` from the main directory, and a
crude option parser will make the examples usable with
one executable:

    $ streaming-network-tcp-examples --help
    Usage: streaming-network-tcp-examples COMMAND

    Available options:
      -h,--help                Show this help text

    Available commands:
      ClientPipeline           
      ClientToUpper            
      ProxyAuth                
      ProxyToUpper             
      ServePipes               
      ServerDouble             
      ServerToUpper

Since most examples use the uppercasing service,
which looks like this:


    serverToUpper :: IO ()
    serverToUpper = do
        putStrLn "Opening upper-casing service on 4000"
        serve (Host "127.0.0.1") "4000" $ \(client,_) -> 
          fromSocket client 4096 -- raw bytes are received from telnet user or the like
          & Q.map toUpper        -- we map them to uppercase
          & toSocket client      -- and send them back


we start it in one terminal:

    term1$ streaming-network-tcp-examples ServerToUpper
    Opening upper-casing service on 4000
    
then in another terminal we can telnet to it:

    term2$ telnet localhost 4000
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    hello -- <-- our input
    HELLO
    ...

or we can scrap telnet and use a dedicated Haskell client, which reads like this:

    clientToUpper :: IO ()
    clientToUpper = connect "127.0.0.1" "4000" $ \(socket,_) -> do
      let act1 = toSocket socket Q.stdin           -- we send our stdin to the service
          act2 = Q.stdout (fromSocket socket 4096) -- we read our stdout from the service
      concurrently act1 act2 
      return ()
    
thus: 

    term3$ streaming-network-tcp-examples ClientToUpper
    el pueblo unido jamas sera vencido!  -- our input
    EL PUEBLO UNIDO JAMAS SERA VENCIDO!
    el pueblo unido jamas sera vencido!  
    EL PUEBLO UNIDO JAMAS SERA VENCIDO!
    ...
    
To complicate things a bit, we can also start
up the doubling service, which looks like this

    serverDoubler :: IO ()
    serverDoubler = do 
      putStrLn "Double server available on 4001"
      serve (Host "127.0.0.1") "4001" $ \(socket, remoteAddr) -> 
        fromSocket socket 4096  -- raw bytes from a client
          & Q.toChunks          -- are munged ...
          & S.map (B.concatMap (\x -> B.pack [x,x]))
          & Q.fromChunks
          & toSocket socket     -- and sent back


thus

     term4$ streaming-network-tcp-examples ServerDouble

then elsewhere

     term5$ telnet localhost 4001
     Trying 127.0.0.1...
     Connected to localhost.
     Escape character is '^]'.
     hello
     hheelllloo

Now let's try the Haskell client that interacts with 4000 and 4001 together,
i.e.:

    clientPipeline :: IO ()
    clientPipeline = do
      putStrLn "We will connect stdin to 4000 and 4001 in succession."
      putStrLn "Input will thus be uppercased and doubled char-by-char.\n"
      connect "127.0.0.1" "4000" $ \(socket1,_) ->
        connect "127.0.0.1" "4001" $ \(socket2,_) ->
          do let act1 = toSocket socket1 Q.stdin
                 -- we send out stdin to the uppercaser
                 act2 = toSocket socket2 (fromSocket socket1 4096)
                 -- we send the results from the uppercase to the doubler
                 act3 = Q.stdout (fromSocket socket2 4096)
                 -- we send the doubler's output to stdout
             runConcurrently $ Concurrently act1 *>  -- all this simultaneously
                               Concurrently act2 *>
                               Concurrently act3

(note the use of the `Applicative` instance for `Concurrently` from the
`async` library), thus:

    term6$ streaming-network-tcp-examples ClientPipeline
    hello
    HHEELLLLOO


Don't tell the children they can access the
uppercasing server directly on localhost 4000; we will
demand authorization on 4003

    term7$ streaming-network-tcp-examples ProxyAuth

which then elsewhere permits

    term8$ telnet localhost 4003
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    Username: spaceballs
    Password: 12345
    Successfully authenticated.
    hello
    HELLO
    hello!
    HELLO!
    

where the program looks like so:


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

      checkAuth :: MonadIO m  => Q.ByteString m r -> Q.ByteString m (Q.ByteString m r)
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


