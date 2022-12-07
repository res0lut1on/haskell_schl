{-# OPTIONS_GHC -Wno-orphans #-}

module Repositories.OrderGR (getOrderByCustomerId, getOrdersByCustomerId) where

import Data.Entities
import Repositories.GRepository
import ReadWrite.ReadWriteOrder ()
import Util.Utilities

instance GenericRepository Order

getOrderByCustomerId :: Int -> IO (Maybe Order)
getOrderByCustomerId sId = maybeHead . filter (\x -> oCId x == sId) <$> getList

getOrdersByCustomerId :: Int -> IO [Order]
getOrdersByCustomerId sId = filter (\x -> oCId x == sId) <$> getList