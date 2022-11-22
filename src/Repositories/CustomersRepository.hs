module Repositories.CustomersRepository
  ( getCustomers,
    getCustomerById,
    getCustomerByOrder,
    getCustomerByOrderId,
  )
where

import Data.Context (orders, сustomers)
import Data.Entities (Customer (..), Order (..))

getCustomers :: [Customer]
getCustomers = сustomers

getCustomerById :: Int -> Customer
getCustomerById sId = head $ filter (\x -> customerId x == sId) сustomers

getCustomerByOrder :: Order -> Customer
getCustomerByOrder (Order _ custId _) = getCustomerById custId

getCustomerByOrderId :: Int -> Customer
getCustomerByOrderId orderID = getCustomerByOrder $ head $ filter (\x -> orderId x == orderID) orders