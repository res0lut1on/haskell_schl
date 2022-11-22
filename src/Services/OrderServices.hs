import Data.Models (OrderModel)
import Mapping.Mapping (mappingOrderToModel)
import Repositories.CustomersRepository (getCustomerByOrderId)
import Repositories.OrderRepository (getOrderById, getOrders)
import Repositories.ProductRepository (getProductsByOrderId)

getOrders :: [OrderModel]
getOrders = map (\o -> mappingOrderToModel o Nothing Nothing) Repositories.OrderRepository.getOrders

getModelOrderById :: Int -> Maybe OrderModel
getModelOrderById orderID =
  let order = getOrderById orderID
      customer = getCustomerByOrderId orderID
      products = Just $ getProductsByOrderId orderID
   in case order of
        Nothing -> Nothing
        Just value -> Just $ mappingOrderToModel value products customer