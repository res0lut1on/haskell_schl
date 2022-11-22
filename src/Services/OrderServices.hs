import Data.Models (OrderModel)
import Mapping.Mapping (mappingOrderToModel)
import Repositories.CustomersRepository (getCustomerByOrderId)
import Repositories.OrderRepository (getOrderById, getOrders)
import Repositories.ProductRepository (getProductsByOrderId)

getOrders :: [OrderModel]
getOrders = map (\o -> mappingOrderToModel o Nothing Nothing) Repositories.OrderRepository.getOrders

getModelOrderById :: Int -> OrderModel
getModelOrderById orderID = mappingOrderToModel (getOrderById orderID) (Just $ getProductsByOrderId orderID) (Just $ getCustomerByOrderId orderID)