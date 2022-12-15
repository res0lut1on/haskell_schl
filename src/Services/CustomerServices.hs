{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Services.CustomerServices (getModelCustomers, getModelCustomerById, addModelCustomer, removeModelCustomer, editModelCustomer) where

import Data.Entities (Customer (customerAddress, customerId, customerName))
import Data.Models (CustomerModel)
import Mapping.Mapping (mappingCustomerToModel, mappingModelToCustomer)
import Repositories.GenericRepository as R

-- getCustomer :: Int -> IO (Maybe Customer)
-- getCustomer = S.get getParam
--   where
--     getParam :: Customer -> IO (Maybe [Order], [(Int, [Product])])
--     getParam cust = R.getOrdersByCustomerId (customerId cust) >>= \ords -> getProductsWithOrdersId >>= \prOrd -> return (Just ords, prOrd)

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
  getEntityById custId >>= \maybeCusts -> 
    case maybeCusts of 
      Nothing -> return Nothing
      Just value -> toMaybeIO $ return (mappingCustomerToModel value) <*> toMaybeIO (getOrdersByCustomerId custId) <*> toMaybeIO getProductsWithOrdersId
  where
    toMaybeIO :: IO a -> IO (Maybe a)
    toMaybeIO value = value >>= \val -> return $ Just val

addModelCustomer :: CustomerModel -> IO Int
addModelCustomer newCust = addEntity $ mappingModelToCustomer newCust

removeModelCustomer :: (GenericRepository a) => Int -> IO (Maybe a)
removeModelCustomer = removeEid

editModelCustomer :: CustomerModel -> IO ()
editModelCustomer newCust = editEntity $ mappingModelToCustomer newCust
