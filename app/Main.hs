module Main where

import qualified Http.SubscriptionRouting as HTTP
import Network.Wai.Handler.Warp (run)
import Servant (serve)
import Data.Proxy (Proxy (..))

proxy :: Proxy HTTP.SubscriptionAPI
proxy = Proxy

main :: IO ()
main = run 8080 . serve proxy $ HTTP.subscriptionServer
-- TODO add some loggin
