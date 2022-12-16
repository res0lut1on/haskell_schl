{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Startup (App (..), AppCache (..), TypeException(..), AppConfig(..)) where

import Control.Monad.Error
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Entities

newtype App a = App
  { runApp :: ExceptT TypeException (WriterT LogMessage (ReaderT AppConfig (StateT AppCache IO))) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader AppConfig,
      MonadState AppCache,
      MonadWriter LogMessage,
      MonadError TypeException
    )

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

data TypeException
  = Error
  | NotFound
  deriving (Eq, Ord, Show)

type LogMessage = [String]