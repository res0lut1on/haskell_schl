{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Startup (App (..), AppCache (..), TypeException (..), AppConfig (..), AppResult (..), run) where

import Control.Monad.Error
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Entities
import qualified Environment as Enviroment

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
  { filePath :: String,
    pageSize :: Int
  }

data TypeException = TypeException
  { text :: String
  }
  deriving (Show)

type LogMessage = [String]

data AppResult a = AppResult
  { logs :: LogMessage,
    state :: AppCache,
    result :: AppData a
  }

data AppData a
  = AppData {appResult :: a}
  | AppError {message :: String}
  deriving (Show)

run :: App a -> IO ((Either TypeException a, LogMessage), AppCache)
run app =
  let config = AppConfig Enviroment.filePath (Enviroment.pageSize)
      appstate = AppCache [] [] [] [] []
      full = runStateT (runReaderT (runWriterT (runExceptT (runApp app))) config) appstate
   in full >>= \((errors, logsApp), stateApp) -> return ((errors, logsApp), stateApp)