{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Startup (App (..), AppCache (..), TypeException (..), AppConfig (..), AppResult (..), run, AppData(..)) where

import Control.Monad.Error
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Entities
import Database.MSSQLServer.Connection (ConnectInfo (..), Connection, connect)
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
    pageSize :: Int,
    connectionString :: Connection
  }

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

newtype TypeException = ElementNotFound String
  deriving (Show)

run :: App a -> IO (AppResult a)
run app =
  let appstate = AppCache [] [] [] [] []
   in do
        conn <- connect Enviroment.defaultConnectInfo
        let config = AppConfig Enviroment.filePath (Enviroment.pageSize) (conn)
        let full = runStateT (runReaderT (runWriterT (runExceptT (runApp app))) config) appstate
        ((errors, logsApp), stateApp) <- full
        return $ res errors logsApp stateApp
  where
    res :: Either TypeException a -> LogMessage -> AppCache -> AppResult a
    res (Left err) logsApp stateApp = AppResult logsApp stateApp (AppError (show err))
    res (Right r) logsApp stateApp = AppResult logsApp stateApp (AppData r)
