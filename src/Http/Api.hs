{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

module Http.Api (API, subscriptionServer) where

import Servant
    ( type (:>),
      Capture,
      Get,
      Delete,
      JSON,
      Server,
      Handler,
      type (:<|>)(..),
      ReqBody, FormUrlEncoded,
      PutCreated, NoContent (NoContent), PostAccepted )
import Data.Text (Text, pack)
import Data.Time (ZonedTime)
import Control.Monad.Cont (MonadIO(liftIO))
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import GHC.TypeLits
import Web.Internal.FormUrlEncoded (FromForm)

type SubscriptionAPI = "subscription" :> 
    (   
    Capture "id" UserId :> Get '[JSON] [Subscription] :<|>
    Capture "id" UserId :> ReqBody '[JSON] Subscription :> PutCreated '[JSON] () :<|>
    Capture "id" UserId :> ReqBody '[JSON] Subscription :> Delete '[JSON] NoContent :<|>
    "slack" :> "command" :> ReqBody '[FormUrlEncoded] SubscriptionCommand :> PostAccepted '[JSON] ()
    )

subscriptionRouter :: 
    (UserId -> Handler [Subscription]) ->
    (UserId -> Subscription -> Handler ()) ->
    (UserId -> Subscription -> Handler NoContent) ->
    (SubscriptionCommand -> Handler ()) ->
    Server SubscriptionAPI

subscriptionRouter allA putA delA postA = allA :<|> putA :<|> delA :<|> postA

type UserId = Text

data Subscription = Subscription
    {
        organization :: Text,
        repository :: Text,
        subscribe_at :: Text -- TODO manage this as a ZonedTime from haskell
    } deriving Generic

data SubscriptionCommand = SubscriptionCommand 
    {
        token :: Text,
        command :: Text,
        text :: Text,
        response_url :: Text,
        trigger_id :: Text,
        user_id :: UserId,
        team_domain :: Text,
        enterprise_id :: Text,
        enterprise_name :: Text,
        channel_id :: Text,
        channel_name :: Text,
        user_name :: Text,
        api_app_id :: Text
    } deriving Generic

instance ToJSON Subscription
instance FromJSON Subscription

instance FromForm SubscriptionCommand

subscriptions :: [Subscription]
subscriptions = [
        Subscription (pack "47deg") (pack "thool") (pack "2021-04-15T12:30:15"),
        Subscription (pack "higherkindness") (pack "skeuomorph") (pack "2021-04-26T15:45:25"),
        Subscription (pack "47degrees") (pack "github4s") (pack "2021-05-01T09:15:05")
    ]
    
subscriptionServer :: Server SubscriptionAPI
subscriptionServer = subscriptionRouter
    (\userId -> return subscriptions) -- TODO build this function differently fofr handling errors
    (\userId subscription -> return ()) -- TODO build this function differently fofr handling errors
    (\userId subscription -> return NoContent) -- TODO build this function differently fofr handling errors
    (\subscriptions -> return ()) -- TODO build this function differently fofr handling errors

-- Main API
type API = SubscriptionAPI


