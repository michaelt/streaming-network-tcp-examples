pipes-network-tcp-examples
==========================

These mechanically follow the pleasantly
transparent 'hello world'-ish examples in
http://www.yesodweb.com/blog/2014/03/network-conduit-async
which mix elementary tcp machinery with elementary
concurrency and conduitry (here, pipe-istry).

The pipes variants follow Michael S's text in this
order:

-   `server_toupper.hs`
    -   a server on 4000 that sends back telnetted
        input upper-cased or 'angry'
-   `server_doubler.hs`
    -   a server on 4001 that sends back telnetted
        input doubled, `Char8` by `Char8`
-   `client_toupper.hs`
    -   a client through which the user interacts
        directly to the "angry" server without
        e.g. calling `telnet`
-   `client_pipeline.hs`
    -   a client that sends material to the
        "angry" server and the doubling server and
        returns it to the user
-   `proxy_toupper.hs`
    -   a proxy on 4002 that sends input to the
        angry server on 4000
-   `proxy_auth.hs`
    -   a proxy on 4003 that asks for demands
        authorization before condescending to send
        user input to the angry server on 4000

Since most examples use the uppercasing service,
we start it in one terminal

    term1$ runhaskell server_toupper.hs

then in another terminal we can write

    term2$ telnet localhost 4000
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    hello
    HELLO

or we can use the direct client

    term3$ runhaskell client_toupper.hs 
    hello
    HELLO

In a flurry of terminal-openings we can also start
up the doubling service

     term4$ runhaskell server_doubler.hs 

then golly

     term5$ telnet localhost 4001
     Trying 127.0.0.1...
     Connected to localhost.
     Escape character is '^]'.
     hello
     hheelllloo

but if we add the client that mixes 4000 and 4001

    term6$ runhaskell client_pipeline.hs 
    hello
    HHEELLLLOO

Don't tell the children they can access the
'angry' server directly on localhost 4000; we will
demand authorization on 4003

    term7$ runhaskell proxy_auth.hs
      

which then permits

     term8$ telnet localhost 4003
     Trying 127.0.0.1...
     Connected to localhost.
     Escape character is '^]'.
     Username: spaceballs
     Password: 12345
     Successfully authenticated.
     joy to the world!
     JOY TO THE WORLD!
