{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Services.CustomerServices (getModelCustomers, getModelCustomerById, addModelCustomer, removeModelCustomer, editModelCustomer, getCustomer) where

import Data.Entities (Customer (customerAddress, customerId, customerName), Order (Order), Product (Product))
import Data.Models (CustomerModel)
import Mapping.Mapping (mappingCustomerToModel, mappingModelToCustomer)
import Repositories.GenericRepository as R
import qualified Services.GService as S
import Startup (App)
import Util.Utilities

getCustomer :: Int -> App CustomerModel
getCustomer = S.get getParam
  where
    getParam :: Customer -> App (Maybe [Order], Maybe [(Int, [Product])])
    getParam cust = R.getOrdersByCustomerId (customerId cust) >>= \ords -> getProductsWithOrdersId >>= \prOrd -> return (Just ords, Just prOrd)

getModelCustomers :: App [CustomerModel]
getModelCustomers =
  getList
    >>= mapM
      ( \o ->
          getOrdersByCustomerId (customerId o) >>= \ord ->
            mappingCustomerToModel o (Just ord) . Just <$> getProductsWithOrdersId
      )

getModelCustomerById :: Int -> App CustomerModel
getModelCustomerById custId =
  getEntityById custId >>= \cust -> (mappingCustomerToModel cust <$> toMaybeM (getOrdersByCustomerId custId)) <*> toMaybeM getProductsWithOrdersId

addModelCustomer :: CustomerModel -> App Int
addModelCustomer newCust = addEntity $ mappingModelToCustomer newCust

removeModelCustomer :: (GenericRepository a) => Int -> App a
removeModelCustomer = removeEid

editModelCustomer :: CustomerModel -> App ()
editModelCustomer newCust = editEntity $ mappingModelToCustomer newCust
