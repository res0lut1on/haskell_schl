{-# LANGUAGE TypeApplications #-}

module Services.OrderServices (getOrders, getModelOrderById, addModelOrder, removeModelOrder, editModelOrder, searchOrders) where

import Data.Entities
import Data.List (isInfixOf)
import Data.Models (OrderModel)
import Data.SearchModel
  ( OrderSearchModel (orderSearchModelNumber),
  )
import Mapping.Mapping (mappingModelToOrder, mappingOrderToModel)
import Repositories.CustomerGR (getCustomerByOrderId)
import Repositories.GRepository
  ( GenericRepository
      ( addEntity,
        editEntity,
        getEntityById,
        getList,
        removeEid,
        search
      ),
  )
import Repositories.ProductGR (getProductsByOrderId)
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

removeModelOrder :: Int -> IO () -- type applicative generic @a
removeModelOrder = removeEid @Order

editModelOrder :: OrderModel -> IO ()
editModelOrder item = editEntity $ mappingModelToOrder item

searchOrders :: OrderSearchModel -> IO [OrderModel]
searchOrders model =
  search filterFunc model >>= \srch ->
    return $ map (\a -> mappingOrderToModel a Nothing Nothing) srch
  where
    filterFunc :: OrderSearchModel -> [Order] -> [Order]
    filterFunc = applyFilter oNumber orderSearchModelNumber isInfixOf