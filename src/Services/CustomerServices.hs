{-# LANGUAGE TypeApplications #-}

module Services.CustomerServices (getModelCustomers, getModelCustomerById, addModelCustomer, removeModelCustomer, editModelCustomer) where

import Data.Entities (Customer (customerId))
import Data.Models (CustomerModel)
import Mapping.Mapping (mappingCustomerToModel, mappingModelToCustomer)
import Repositories.GenericRepository

getModelCustomers :: IO [CustomerModel]
getModelCustomers =
  do
    custs <- getList
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
    customer <- getEntityById custId
    prods <- getProductsWithOrdersId
    case customer of
      Nothing -> return Nothing
      Just value -> return $ Just $ mappingCustomerToModel value (Just orders) (Just prods)

addModelCustomer :: CustomerModel -> IO Int
addModelCustomer newCust = addEntity $ mappingModelToCustomer newCust

removeModelCustomer :: Int -> IO ()
removeModelCustomer = removeEid @Customer

editModelCustomer :: CustomerModel -> IO ()
editModelCustomer newCust = editEntity $ mappingModelToCustomer newCust