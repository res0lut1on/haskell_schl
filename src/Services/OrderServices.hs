{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Services.OrderServices (getOrders, getModelOrderById, addModelOrder, removeModelOrder, editModelOrder) where

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
import Repositories.ProductGR (getProductsByOrderId)

-- getOrder :: Int -> IO (Maybe OrderModel)
-- getOrder = S.get getParam
--   where
--     getParam :: Order -> IO (Maybe [Product], Customer) -- у заказа же всегда есть Customer? Или нужно сделать тоже Maybe Customer
--     getParam ord = R.getCustomerByOrder ord >>= \maybeCustomer -> R.getProductsByOrder ord >>= \prods -> return (Just prods, maybeCustomer)

getOrders :: IO [OrderModel]
getOrders = map (\o -> mappingOrderToModel o Nothing Nothing) <$> getList

getModelOrderById :: Int -> IO (Maybe OrderModel)
getModelOrderById orderID =
  let prods = getProductsByOrderId orderID
      cust = getCustomerByOrderId orderID
      ords = getEntityById orderID
   in ords >>= \maybeOrd -> case maybeOrd of
        Nothing -> return Nothing
        Just value -> toMaybeIO $ return (mappingOrderToModel value) <*> toMaybeIO prods <*> cust
  where
    toMaybeIO :: IO a -> IO (Maybe a)
    toMaybeIO value = value >>= \val -> return $ Just val

addModelOrder :: OrderModel -> IO Int
addModelOrder item = addEntity $ mappingModelToOrder item

removeModelOrder :: (GenericRepository a) => Int -> IO (Maybe a)
removeModelOrder = removeEid

editModelOrder :: OrderModel -> IO ()
editModelOrder item = editEntity $ mappingModelToOrder item
