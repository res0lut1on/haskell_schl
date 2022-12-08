{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Repositories.CustomerGR (getCustomerByOrderId, getCustomerByOrder) where

import Data.Entities
import ReadWrite.ReadWriteCustomer ()
import Repositories.GRepository
import Repositories.OrderGR ()

instance GenericRepository Customer

getCustomerByOrder :: Order -> IO (Maybe Customer)
getCustomerByOrder (Order _ custId _) = getEntityById custId

getCustomerByOrderId :: Int -> IO (Maybe Customer)
getCustomerByOrderId orderID =
  (getList @Order) >>= \ords -> getCustomerByOrder $ head (filter (\x -> orderId x == orderID) ords)