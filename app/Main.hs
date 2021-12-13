module Main where

import Network.Wai.Handler.Warp (run)
import Servant (serve)
import Data.Proxy (Proxy (..))
import qualified Http.Api as HTTP
import qualified Postgres.Migration as Migration
import qualified Postgres.Config as DB

proxy :: Proxy HTTP.API
proxy = Proxy

main :: IO ()
main = do
    pool <- DB.create 
    migration <- Migration.migrate pool "db/migrations"
    run 8080 . serve proxy $ HTTP.subscriptionServer
-- TODO add some loggin
