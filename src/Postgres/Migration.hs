module Postgres.Migration where
import Database.PostgreSQL.Simple (connectPostgreSQL, withTransaction)
import Database.PostgreSQL.Simple.Migration (MigrationContext(MigrationContext), MigrationCommand (MigrationDirectory), runMigration, MigrationResult)
import qualified Data.ByteString.Char8 as BS

data PgConfig = PgConfig {
    host :: String,
    port :: Integer,
    database :: String,
    user :: String,
    password :: String
}

migrate :: IO (MigrationResult String)
migrate = do
    let url = "host=localhost dbname=github_subscription user=postgres password=haskell"
    let dir = "db/migrations"
    con <- connectPostgreSQL (BS.pack url)
    withTransaction con $ runMigration $
        MigrationContext (MigrationDirectory dir) False con