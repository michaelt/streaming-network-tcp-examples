streaming-network-tcp-examples
==============================

These mechanically follow the pleasantly
transparent 'hello world'-ish examples in
http://www.yesodweb.com/blog/2014/03/network-conduit-async
which mix networking ABCs with elementary
concurrency and conduitry.

Apart from `pipes-network` and `async` we use the
`word8` library for humane word8 handling. 

The variants follow Michael S's text in this
order:

-   `Examples/ServerToUpper.hs`
    -   a server on 4000 that sends back input sent e.g. with telnet
        upper-cased or 'angry'
-   `Examples/ServerDouble.hs`
    -   a server on 4001 that sends back 
        input doubled, `Char8` by `Char8`
-   `Examples/ClientToUpper.hs`
    -   a client through which the user interacts
        directly to the "angry" server 
-   `Examples/ClientPipeline.hs`
    -   a client that sends material to the
        "angry" server and the doubling server and
        returns it to the user
-   `Examples/ProxyToUpper.hs`
    -   a proxy on 4002 that sends input to the
        angry server on 4000
-   `Examples/ProxyAuth.hs`
    -   a proxy on 4003 that asks for demands
        authorization before condescending to send
        user input to the angry server on 4000

The following remarks will require that eight
instances of a terminal all be opened in the main
directory of the repository; if you like you can
`cabal install` from the main directory, and a
crude option parser will make the examples usable with
one executable:

    $ pipes-network-tcp-examples --help
    Usage: pipes-network-tcp-examples COMMAND

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
          toSocket client $ Q.map toUpper $ fromSocket client 4096 
         

we start it in one terminal:


    term1$ streaming-network-examples ServerToUpper
    Opening upper-casing service on 4000
    
then in another terminal we can write

    term2$ telnet localhost 4000
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    hello -- <-- our input
    HELLO
    ...

or we can scrap telnet and use a dedicated Haskell client, which reads like this:

      clientToUpper :: IO ()
      clientToUpper = connect "127.0.0.1" "4000" $ \(connectionSocket,_) -> do
        let act1 = toSocket connectionSocket Q.stdin  
            act2 = Q.stdout (fromSocket connectionSocket 4096) 
        concurrently act1 act2 
        return ()
      

thus: 

    term3$ streaming-network-examples ClientToUpper
    el pueblo unido jamas sera vencido!  -- our input
    EL PUEBLO UNIDO JAMAS SERA VENCIDO!
    el pueblo unido jamas sera vencido!  
    EL PUEBLO UNIDO JAMAS SERA VENCIDO!
    ...
    
In a flurry of terminal-openings we can also start
up the doubling service, which looks like this

    serverDoubler :: IO ()
    serverDoubler = do 
      putStrLn "Double server available on 4001"
      serve (Host "127.0.0.1") "4001" $ \(connectionSocket, remoteAddr) -> 
        fromSocket connectionSocket 4096
              & Q.toChunks
              & S.map (B.concatMap (\x -> B.pack [x,x]))
              & Q.fromChunks
              & toSocket connectionSocket


thus

     term4$ streaming-network-examples ServerDouble

then elsewhere

     term5$ telnet localhost 4001
     Trying 127.0.0.1...
     Connected to localhost.
     Escape character is '^]'.
     hello
     hheelllloo

But let's try the Haskell client that interacts with 4000 and 4001 together,
i.e.:

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

(note the use of the `Applicative` instance for `Concurrently` from the
`async` library), thus:

    term6$ streaming-network-examples ClientPipeline
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


