module Repositories.CustomersRepository
  ( getCustomers,
    getCustomerById,
    getCustomerByOrder,
    getCustomerByOrderId,
  )
where

import Data.Context (сustomers)
import Data.Entities (Customer (..), Order (..))
import Repositories.OrderRepository
import Utilities (maybeHead)

getCustomers :: [Customer]
getCustomers = сustomers

getCustomerById :: Int -> Maybe Customer
getCustomerById sId = maybeHead $ filter (\x -> customerId x == sId) getCustomers

getCustomerByOrder :: Order -> Maybe Customer
getCustomerByOrder (Order _ custId _) = getCustomerById custId

getCustomerByOrderId :: Int -> Maybe Customer
getCustomerByOrderId orderID = getCustomerByOrder $ head $ filter (\x -> orderId x == orderID) getOrders
