module Main where
  
import Options.Applicative
import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Examples.ClientPipeline as ClientPipeline
import qualified Examples.ClientToUpper  as ClientToUpper
import qualified Examples.ProxyAuth      as ProxyAuth
import qualified Examples.ProxyToUpper   as ProxyToUpper
import qualified Examples.ServePipes     as ServePipes
import qualified Examples.ServerDouble   as ServerDouble
import qualified Examples.ServerToUpper  as ServerToUpper



opts :: Parser (IO ())
opts = helper <*> subparser stuff where 
   stuff = mconcat
    [ command "ClientPipeline" (info (pure ClientPipeline.main) idm)
    , command "ClientToUpper"  (info (pure ClientToUpper.main) idm)
    , command "ProxyAuth"      (info (pure ProxyAuth.main) idm)
    , command "ProxyToUpper"   (info (pure ProxyToUpper.main) idm)
    , command "ServePipes"     (info (pure ServePipes.main) idm)
    , command "ServerDouble"   (info (pure ServerDouble.main) idm)
    , command "ServerToUpper"  (info (pure ServerToUpper.main) idm)
    ]


main :: IO ()
main = join $ execParser (info opts idm)