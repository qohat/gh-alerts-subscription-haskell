module Service.Subscriptions where

import qualified Domain.Subscription as DOMAIN
import Data.Text ( pack )

class (Monad m) => SubscriptionService m where
    get :: DOMAIN.UserId -> m [DOMAIN.Subscription]
    subscribe :: DOMAIN.UserId -> DOMAIN.Subscription -> m ()
    unsubscribe :: DOMAIN.UserId -> m ()

instance SubscriptionService IO where 
  get id' = pure subscriptions 
  subscribe id' subs' = pure ()
  unsubscribe id' = pure ()


subscriptions :: [DOMAIN.Subscription]
subscriptions = [
        DOMAIN.Subscription (pack "47deg") (pack "thool") (pack "2021-04-15T12:30:15"),
        DOMAIN.Subscription (pack "higherkindness") (pack "skeuomorph") (pack "2021-04-26T15:45:25"),
        DOMAIN.Subscription (pack "47degrees") (pack "github4s") (pack "2021-05-01T09:15:05")
    ]
