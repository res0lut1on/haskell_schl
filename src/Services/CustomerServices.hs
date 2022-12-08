{-# LANGUAGE TypeApplications #-}

module Services.CustomerServices (getModelCustomers, getModelCustomerById, addModelCustomer, removeModelCustomer, editModelCustomer) where

import Data.Entities (Customer (customerId))
import Data.Models (CustomerModel)
import Mapping.Mapping (mappingCustomerToModel, mappingModelToCustomer)
import Repositories.GenericRepository

getModelCustomers :: IO [CustomerModel]
getModelCustomers =
  getList
    >>= mapM
      ( \o ->
          getOrdersByCustomerId (customerId o) >>= \ord ->
            mappingCustomerToModel o (Just ord) . Just <$> getProductsWithOrdersId
      )

getModelCustomerById :: Int -> IO (Maybe CustomerModel)
getModelCustomerById custId =
  getOrdersByCustomerId custId >>= \orders ->
    getEntityById custId
      >>= \customer ->
        getProductsWithOrdersId >>= \prods ->
          case customer of
            Nothing -> return Nothing
            Just value -> return $ Just $ mappingCustomerToModel value (Just orders) (Just prods)

-- getModelCustomerById :: Int -> IO (Maybe CustomerModel)
-- getModelCustomerById custId =
--     getEntityById custId >>= \customer ->
--           case customer of
--             Nothing -> return Nothing
--             --Just value -> return $ Just $ mappingCustomerToModel value (Just orders) (Just prods)
--             Just value ->  return $ Just $ mappingCustomerToModel <$> value <*> (Just <$> getOrdersByCustomerId 1) <*> (Just <$> getEntityById custId)

addModelCustomer :: CustomerModel -> IO Int
addModelCustomer newCust = addEntity $ mappingModelToCustomer newCust

removeModelCustomer :: Int -> IO ()
removeModelCustomer = removeEid @Customer

editModelCustomer :: CustomerModel -> IO ()
editModelCustomer newCust = editEntity $ mappingModelToCustomer newCust