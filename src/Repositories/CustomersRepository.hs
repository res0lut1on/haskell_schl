module Repositories.CustomersRepository
  ( getCustomers,
    getCustomerById,
    getCustomerByOrder,
    getCustomerByOrderId,
    addCust,
    removeCust,
    addCustomer,
    removeCustomer,
    updateCustomer,
  )
where

import Data.Entities (Customer (..), Order (..))
import Repositories.OrderRepository
import Util.FileUtil
import Util.Utilities (maybeHead)

getCustomers :: IO [Customer]
getCustomers = readAllCustomers

getCustomerById :: Int -> IO (Maybe Customer)
getCustomerById sId = maybeHead . filter (\x -> customerId x == sId) <$> getCustomers

getCustomerByOrder :: Order -> IO (Maybe Customer)
getCustomerByOrder (Order _ custId _) = getCustomerById custId

getCustomerByOrderId :: Int -> IO (Maybe Customer)
getCustomerByOrderId orderID =
  do
    ords <- getOrd
    getCustomerByOrder $ head $ filter (\x -> orderId x == orderID) ords

addCustomer :: Customer -> IO Int
addCustomer newCust =
  do
    allCust <- readAllCustomers
    let prevCust = last allCust
    let cId = customerId prevCust + 1
    addCust
      ( Customer
          { customerId = cId,
            customerName = customerName newCust,
            customerAddress = customerAddress newCust
          }
      )
    return cId

removeCustomer :: Int -> IO ()
removeCustomer = removeCust 

updateCustomer :: Customer -> IO ()
updateCustomer = updateCust
