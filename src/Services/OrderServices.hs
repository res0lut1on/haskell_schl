{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Services.OrderServices (getOrders, addModelOrder, removeModelOrder, editModelOrder, getModelOrderById, getOrder) where

import Data.Entities
import Data.Models (OrderModel)
import Mapping.Mapping (mappingModelToOrder, mappingOrderToModel)
import Repositories.CustomerGR (getCustomerByOrderId)
import Repositories.GRepository
  ( GenericRepository
      ( addEntity,
        editEntity,
        getEntityById,
        getList,
        removeEid
      ),
  )
import qualified Repositories.GenericRepository as R
import Repositories.ProductGR (getProductsByOrderId)
import qualified Services.GService as S
import Startup
import Util.Utilities (toMaybeM)

getOrder :: Int -> App OrderModel
getOrder = S.get getParam
  where
    getParam :: Order -> App (Maybe [Product], Customer)
    getParam ord = R.getCustomerByOrder ord >>= \maybeCustomer -> R.getProductsByOrder ord >>= \prods -> return (Just prods, maybeCustomer)

getOrders :: App [OrderModel]
getOrders = map (\o -> mappingOrderToModel o Nothing Nothing) <$> getList

getModelOrderById :: Int -> App OrderModel
getModelOrderById orderID =
  let prods = getProductsByOrderId orderID
      cust = getCustomerByOrderId orderID
      ords = getEntityById orderID
   in (mappingOrderToModel <$> ords) <*> toMaybeM prods <*> toMaybeM cust

addModelOrder :: OrderModel -> App Int
addModelOrder item = addEntity $ mappingModelToOrder item

removeModelOrder :: (GenericRepository a) => Int -> App a
removeModelOrder = removeEid

editModelOrder :: OrderModel -> App ()
editModelOrder item = editEntity $ mappingModelToOrder item
