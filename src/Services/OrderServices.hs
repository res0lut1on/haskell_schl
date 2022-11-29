module Services.OrderServices (getOrders, getModelOrderById, addModelOrder, removeModelOrder, editModelOrder) where

import Data.Models (OrderModel)
import Mapping.Mapping (mappingModelToOrder, mappingOrderToModel)
import Repositories.CustomersRepository (getCustomerByOrderId, getCustomers)
import Repositories.OrderRepository (addOrder, getOrd, getOrderById, removeOrder, updateOrder)
import Repositories.ProductRepository (getProductsByOrderId)
import Util.FileUtil

getOrders :: IO [OrderModel]
getOrders = map (\o -> mappingOrderToModel o Nothing Nothing) <$> Repositories.OrderRepository.getOrd

getModelOrderById :: Int -> IO (Maybe OrderModel)
getModelOrderById orderID =
  do
    order <- getOrderById orderID
    customer <- getCustomerByOrderId orderID
    products <- getProductsByOrderId orderID
    case order of
      Nothing -> return Nothing
      Just value -> return $ Just $ mappingOrderToModel value (Just products) customer

addModelOrder :: OrderModel -> IO Int
addModelOrder item = addOrder $ mappingModelToOrder item

removeModelOrder :: Int -> IO ()
removeModelOrder = removeOrder

editModelOrder :: OrderModel -> IO ()
editModelOrder item = updateOrder $ mappingModelToOrder item