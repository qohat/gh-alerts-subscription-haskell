{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Http.SubscriptionRouting where

-- import Servant (Server, type (:<|>) (..), type (:>))
import Servant
import Data.Text (Text, pack)
import Data.Time (ZonedTime)
import Control.Monad.Cont (MonadIO(liftIO))
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

data Subscription = Subscription
    {
        organization :: Text,
        repository :: Text,
        subscribe_at :: Text -- TODO manage this as a ZonedTime from haskell
    } deriving Generic

instance ToJSON Subscription

subscriptions :: [Subscription]
subscriptions = [
        Subscription (pack "47deg") (pack "thool") (pack "2021-04-15T12:30:15"),
        Subscription (pack "higherkindness") (pack "skeuomorph") (pack "2021-04-26T15:45:25"),
        Subscription (pack "47degrees") (pack "github4s") (pack "2021-05-01T09:15:05")
    ]

type SubscriptionAPI = "subscription" :> Capture "userId" Text :> Get '[JSON] [Subscription]

subscriptionServer :: Server SubscriptionAPI
subscriptionServer userId = return subscriptions

