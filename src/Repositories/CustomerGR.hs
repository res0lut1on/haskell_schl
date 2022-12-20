{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Repositories.CustomerGR (getCustomerByOrderId, getCustomerByOrder) where

import Control.Monad.Writer (MonadWriter (tell))
import Data.DataBase.ReadWriteDataBase.RWDBCustomer ()
import Data.Entities
import ReadWrite.ReadWriteCustomer ()
import Repositories.GRepository
import Repositories.OrderGR ()
import Startup

instance GenericRepository Customer

getCustomerByOrder :: Order -> App Customer
getCustomerByOrder (Order _ custId _) = tell ["getCustomerByOrder begin"] >> getEntityById custId >>= \res -> tell ["getCustomerByOrder end"] >> return res

getCustomerByOrderId :: Int -> App Customer
getCustomerByOrderId orderID =
  tell ["getCustomerByOrderId begin"]
    >> (getList @Order)
    >>= (getCustomerByOrder . head) . filter (\x -> orderId x == orderID)
    >>= \res -> tell ["getCustomerByOrderId end"] >> return res