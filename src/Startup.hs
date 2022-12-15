module Startup (App (..), AppCache(..)) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Entities

newtype App a = App
  { runApp :: WriterT LogMessage (ReaderT AppConfig (StateT AppCache IO)) a
  }

data AppCache = AppCache
  { customerCache :: [Customer],
    orderCache :: [Order],
    shopCache :: [Shop],
    productCache :: [Data.Entities.Product],
    productOrderCache :: [ProductOrder]
  }

data AppConfig = AppConfig
  { pageSize :: Int,
    filePath :: String
  }



type LogMessage = String