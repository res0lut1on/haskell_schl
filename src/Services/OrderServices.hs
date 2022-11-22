import Data.Models (OrderModel)
import Lib

getOrders :: [OrderModel]
getOrders = Lib.map (\p -> OrderModel 1 1 1 Nothing) OrdersRepository.getOrders