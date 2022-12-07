{-# LANGUAGE TypeApplications #-}

module Services.OrderServices (getOrders, getModelOrderById, addModelOrder, removeModelOrder, editModelOrder) where

import Data.Entities
import Data.Models (OrderModel)
import Mapping.Mapping (mappingModelToOrder, mappingOrderToModel)
import Repositories.GenericRepository

getOrders :: IO [OrderModel]
getOrders = map (\o -> mappingOrderToModel o Nothing Nothing) <$> getList

getModelOrderById :: Int -> IO (Maybe OrderModel)
getModelOrderById orderID =
  do
    order <- getEntityById orderID
    customer <- getCustomerByOrderId orderID
    products <- getProductsByOrderId orderID
    case order of
      Nothing -> return Nothing
      Just value -> return $ Just $ mappingOrderToModel value (Just products) customer

addModelOrder :: OrderModel -> IO Int
addModelOrder item = addEntity $ mappingModelToOrder item

removeModelOrder :: Int -> IO ()
removeModelOrder = removeEid @Order

editModelOrder :: OrderModel -> IO ()
editModelOrder item = editEntity $ mappingModelToOrder item