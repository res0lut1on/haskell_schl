{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Repositories.CustomerGR (getCustomerByOrderId, getCustomerByOrder) where

import Data.Entities
import ReadWrite.ReadWriteCustomer ()
import Repositories.GRepository
import Repositories.OrderGR ()
import Startup
import Control.Monad.Writer (MonadWriter(tell))

instance GenericRepository Customer

getCustomerByOrder :: Order -> App (Maybe Customer)
getCustomerByOrder (Order _ custId _) = tell ["getCustomerByOrder begin"] >> getEntityById custId >>= \res -> tell ["getCustomerByOrder end"] >> return res

getCustomerByOrderId :: Int -> App (Maybe Customer)
getCustomerByOrderId orderID =
  tell ["getCustomerByOrderId begin"] >> 
  (getList @Order) >>= (getCustomerByOrder . head) . filter (\x -> orderId x == orderID) >>= \res -> tell ["getCustomerByOrderId end"] >> return res