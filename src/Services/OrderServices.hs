{-# LANGUAGE TypeApplications #-}

module Services.OrderServices (getOrders, getModelOrderById, addModelOrder, removeModelOrder, editModelOrder, searchOrders) where

import Data.Entities
import Data.List
import Data.Models (OrderModel)
import Data.SearchModel (OrderSearchModel (OrderSearchModel, orderSearchModelNumber))
import Mapping.Mapping (mappingModelToOrder, mappingOrderToModel)
import Repositories.GenericRepository
import Services.ApplyFilter

getOrders :: IO [OrderModel]
getOrders = map (\o -> mappingOrderToModel o Nothing Nothing) <$> getList

getModelOrderById :: Int -> IO (Maybe OrderModel)
getModelOrderById orderID =
  getEntityById orderID >>= \maybeOrder ->
    getCustomerByOrderId orderID >>= \customer ->
      getProductsByOrderId orderID >>= \products ->
        return $
          maybeOrder >>= \order ->
            return $ mappingOrderToModel order (Just products) customer

addModelOrder :: OrderModel -> IO Int
addModelOrder item = addEntity $ mappingModelToOrder item

removeModelOrder :: Int -> IO ()
removeModelOrder = removeEid @Order

editModelOrder :: OrderModel -> IO ()
editModelOrder item = editEntity $ mappingModelToOrder item

searchOrders :: OrderSearchModel -> IO [OrderModel]
searchOrders model =
  search filterFunc model >>= \srch ->
    return $ map (\a -> mappingOrderToModel a Nothing Nothing) srch
  where
    filterFunc :: OrderSearchModel -> [Order] -> [Order]
    filterFunc filters =
      applyFilter oNumber orderSearchModelNumber isInfixOf filters