{-# OPTIONS_GHC -Wno-orphans #-}

module Repositories.OrderGR (getOrderByCustomerId, getOrdersByCustomerId) where

import Data.Entities
import Repositories.GRepository
import ReadWrite.ReadWriteOrder ()
import Util.Utilities
import Startup
import Control.Monad.Writer (MonadWriter(tell))

instance GenericRepository Order

getOrderByCustomerId :: Int -> App (Maybe Order)
getOrderByCustomerId sId = tell ["getOrderByCustomerId begin"] >> maybeHead . filter (\x -> oCId x == sId) <$> getList >>= \res -> tell ["getOrderByCustomerId end"] >> return res

getOrdersByCustomerId :: Int -> App [Order]
getOrdersByCustomerId sId = tell ["getOrdersByCustomerId begin"] >> filter (\x -> oCId x == sId) <$> getList >>= \res -> tell ["getOrdersByCustomerId end"] >> return res