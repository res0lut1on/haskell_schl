{-# LANGUAGE TypeApplications #-}

module Services.CustomerServices (getModelCustomers, getModelCustomerById, addModelCustomer, removeModelCustomer, editModelCustomer, getCustomer) where

import Data.Entities (Customer (customerAddress, customerId, customerName), Order, Product (Product))
import Data.Models (CustomerModel)
import Mapping.Mapping (mappingCustomerToModel, mappingModelToCustomer)
import Repositories.GenericRepository as R
import qualified Services.GService as S

getCustomer :: Int -> IO (Maybe Customer)
getCustomer = S.get getParam
  where
    getParam :: Customer -> IO (Maybe [Order], [(Int, [Product])])
    getParam cust = R.getOrdersByCustomerId (customerId cust) >>= \ords -> getProductsWithOrdersId >>= \prOrd -> return (Just ords, prOrd)














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

removeModelCustomer :: (GenericRepository a) => Int -> IO (Maybe a)
removeModelCustomer = removeEid

editModelCustomer :: CustomerModel -> IO ()
editModelCustomer newCust = editEntity $ mappingModelToCustomer newCust
