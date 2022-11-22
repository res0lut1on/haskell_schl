module Repositories.CustomersRepository
  ( getCustomers,
    getCustomerById,
  )
where

import Data.Context (сustomers)
import Data.Entities (Customer (..))

getCustomers :: [Customer]
getCustomers = сustomers

getCustomerById :: Int -> Customer
getCustomerById sId = head $ filter (\x -> customerId x == sId) сustomers
