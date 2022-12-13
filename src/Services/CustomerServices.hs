{-# LANGUAGE TypeApplications #-}

module Services.CustomerServices (getModelCustomers, getModelCustomerById, addModelCustomer, removeModelCustomer, editModelCustomer, searchCustomers) where

import Data.Entities (Customer (customerAddress, customerId, customerName))
import Data.List
import Data.Models (CustomerModel)
import Data.SearchModel (CustomerSearchModel (customerSearchModelAddress, customerSearchModelName))
import Mapping.Mapping (mappingCustomerToModel, mappingModelToCustomer)
import Repositories.GenericRepository
import Services.ApplyFilter

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
      >>= \maybeCustomer ->
        getProductsWithOrdersId >>= \prods ->
          return $
            maybeCustomer >>= \customer ->
              return $ mappingCustomerToModel customer (return orders) (return prods) -- <$>  --desugaring in book 

-- getModelCustomerById :: Int -> IO (Maybe CustomerModel)
-- getModelCustomerById custId = getEntityById custId >>= \maybeCusts -> return (mappingCustomerToModel <$> maybeCusts <*> getOrdersByCustomerId custId <*> getProductsWithOrdersId custId)

addModelCustomer :: CustomerModel -> IO Int
addModelCustomer newCust = addEntity $ mappingModelToCustomer newCust

removeModelCustomer :: Int -> IO ()
removeModelCustomer = removeEid @Customer

editModelCustomer :: CustomerModel -> IO ()
editModelCustomer newCust = editEntity $ mappingModelToCustomer newCust

searchCustomers :: CustomerSearchModel -> IO [CustomerModel]
searchCustomers model =
  map (\a -> mappingCustomerToModel a Nothing Nothing) <$> search filterFunc model
  where
    filterFunc :: CustomerSearchModel -> [Customer] -> [Customer]
    filterFunc filters =
      applyFilter customerName customerSearchModelName isInfixOf filters
        . applyFilter customerAddress customerSearchModelAddress isInfixOf filters
