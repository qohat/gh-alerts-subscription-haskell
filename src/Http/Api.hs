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
      ReqBody,
      PutCreated, NoContent (NoContent) )
import Data.Text (Text, pack)
import Data.Time (ZonedTime)
import Control.Monad.Cont (MonadIO(liftIO))
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import GHC.TypeLits

type SimpleAPI (path :: Symbol) a i = path :> 
    (   
    Capture "id" i :> Get '[JSON] [a] :<|>
    Capture "id" i :> ReqBody '[JSON] a :> PutCreated '[JSON] () :<|>
    Capture "id" i :> ReqBody '[JSON] a :> Delete '[JSON] NoContent
    )

simpleServer :: 
    (i -> Handler [a]) ->
    (i -> a -> Handler ()) ->
    (i -> a -> Handler NoContent) ->
    Server (SimpleAPI name a i)

simpleServer allA putA delA = allA :<|> putA :<|> delA

type UserId = Text

data Subscription = Subscription
    {
        organization :: Text,
        repository :: Text,
        subscribe_at :: Text -- TODO manage this as a ZonedTime from haskell
    } deriving Generic

instance ToJSON Subscription
instance FromJSON Subscription

subscriptions :: [Subscription]
subscriptions = [
        Subscription (pack "47deg") (pack "thool") (pack "2021-04-15T12:30:15"),
        Subscription (pack "higherkindness") (pack "skeuomorph") (pack "2021-04-26T15:45:25"),
        Subscription (pack "47degrees") (pack "github4s") (pack "2021-05-01T09:15:05")
    ]
    
subscriptionServer :: Server (SimpleAPI "subscription" Subscription UserId)
subscriptionServer = simpleServer
    (\userId -> return subscriptions) -- TODO build this function differently fofr handling errors
    (\userId subscription -> return ()) -- TODO build this function differently fofr handling errors
    (\userId subscription -> return NoContent) -- TODO build this function differently fofr handling errors

-- Main API
type API = SimpleAPI "subscription" Subscription UserId


