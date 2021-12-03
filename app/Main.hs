module Main where

import Network.Wai.Handler.Warp (run)
import Servant (serve)
import Data.Proxy (Proxy (..))
import qualified Http.Api as HTTP

proxy :: Proxy HTTP.API
proxy = Proxy

main :: IO ()
main = run 8080 . serve proxy $ HTTP.subscriptionServer
-- TODO add some loggin
