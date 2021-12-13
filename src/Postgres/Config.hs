module Postgres.Config where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Pool (Pool, createPool)
import qualified Database.PostgreSQL.Simple as PG
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

data PgConfig = PgConfig {
    host :: String,
    port :: Integer,
    database :: String,
    user :: String,
    password :: String
}

loadPgConfig :: MonadIO m => m PgConfig
loadPgConfig = do
  host' <- liftIO $ lookupEnv "DB_HOST"
  port' <- liftIO $ lookupEnv "DB_PORT"
  user' <- liftIO $ lookupEnv "DB_USER"
  pass' <- liftIO $ lookupEnv "DB_PASS"
  name' <- liftIO $ lookupEnv "DB_NAME"
  return
    PgConfig {
      host = fromMaybe "localhost" host',
      port = read (fromMaybe "5432" port') :: Integer,
      user = fromMaybe "postgres" user',
      password = fromMaybe "haskell" pass',
      database = fromMaybe "github_subscription" name'
    }

create :: MonadIO m => m (Pool PG.Connection)
create = do
  conf <- loadPgConfig
  liftIO $ createPool (connection conf) close 1 10 15

connection :: MonadIO m => PgConfig -> m PG.Connection
connection conf =
  liftIO $
    PG.connect
      PG.ConnectInfo
        { PG.connectHost = host conf,
          PG.connectPort = fromIntegral (port conf),
          PG.connectUser = user conf,
          PG.connectPassword = password conf,
          PG.connectDatabase = database conf
        }

close :: MonadIO m => PG.Connection -> m ()
close conn = liftIO $ PG.close conn