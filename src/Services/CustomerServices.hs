module Services.CustomerServices (getModelCustomers, getModelCustomerById, addModelCustomer, removeModelCustomer, editModelCustomer) where

import Data.Entities (Customer (customerId))
import Data.Models (CustomerModel)
import Mapping.Mapping (mappingCustomerToModel, mappingModelToCustomer)
import Repositories.CustomersRepository (addCustomer, getCustomerById, removeCustomer, updateCustomer)
import qualified Repositories.CustomersRepository as CustomersRepository
import Repositories.OrderRepository (getOrdersByCustomerId)
import Repositories.ProductRepository (getProductsWithOrdersId)

getModelCustomers :: IO [CustomerModel]
getModelCustomers =
  do
    custs <- CustomersRepository.getCustomers
    mapM
      ( \o ->
          do
            ord <- getOrdersByCustomerId (customerId o)
            mappingCustomerToModel o (Just ord) . Just <$> getProductsWithOrdersId
      )
      custs

getModelCustomerById :: Int -> IO (Maybe CustomerModel)
getModelCustomerById custId =
  do
    orders <- getOrdersByCustomerId custId
    customer <- getCustomerById custId
    prods <- getProductsWithOrdersId
    case customer of
      Nothing -> return Nothing
      Just value -> return $ Just $ mappingCustomerToModel value (Just orders) (Just prods)

addModelCustomer :: CustomerModel -> IO Int
addModelCustomer newCust = addCustomer $ mappingModelToCustomer newCust

removeModelCustomer :: Int -> IO ()
removeModelCustomer = removeCustomer

editModelCustomer :: CustomerModel -> IO ()
editModelCustomer newCust = updateCustomer $ mappingModelToCustomer newCust