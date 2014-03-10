pipes-network-tcp-examples
==========================

These mechanically follow http://www.yesodweb.com/blog/2014/03/network-conduit-async 
The pipes variants follow Michael S's text in this order:

- `server_toupper.hs` (a server on 4000 that sends back telnetted input 
    upper-cased or 'angry')
- `server_doubler.hs` (a server on 4001 that sends back telnetted input 
    doubled, `Char8` by `Char8`)
- `client_toupper.hs` (a client through which the user interacts 
     directly to the "angry" server without e.g. calling `telnet`)
- `client_pipeline.hs` (a client that sends material to the "angry" 
    server and the doubling server and returns it to the user)
- `proxy_toupper.hs` (a proxy on 4002 that sends input to the angry server on 4000 )
- `proxy_auth.hs` (a proxy on 4003 that asks for demands authorization before
    condescending to send user input to the angry server on 4000)


So for example

     terminal1$ runhaskell server_toupper.hs

and elsewhere

     terminal2$ runhaskell proxy_auth.hs

will permit 

     terminal3$ telnet localhost 4003
     Trying 127.0.0.1...
     Connected to localhost.
     Escape character is '^]'.
     Username: spaceballs
     Password: 12345
     Successfully authenticated.
     joy to the world!
     JOY TO THE WORLD!