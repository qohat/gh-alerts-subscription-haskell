{-# LANGUAGE DeriveGeneric #-}

module Domain.Subscription where
import Data.Text ( Text ) 
import GHC.Generics ( Generic )

type UserId = Text

data Subscription = Subscription
    {
        organization :: Text,
        repository :: Text,
        subscribe_at :: Text -- TODO manage this as a ZonedTime from haskell
    } deriving Generic