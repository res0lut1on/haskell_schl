{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}

module Repositories.CustomerGR (getCustomerByOrderId, getCustomerByOrder) where

import Data.Entities
import Repositories.GRepository
import Repositories.OrderGR ()
import ReadWrite.ReadWriteCustomer ()

instance GenericRepository Customer

getCustomerByOrder :: Order -> IO (Maybe Customer)
getCustomerByOrder (Order _ custId _) = getEntityById custId

getCustomerByOrderId :: Int -> IO (Maybe Customer)
getCustomerByOrderId orderID =
  do
    ords <- getList @Order
    getCustomerByOrder $ head $ filter (\x -> orderId x == orderID) ords