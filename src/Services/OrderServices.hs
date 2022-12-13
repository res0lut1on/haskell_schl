module Services.OrderServices (getOrders, getModelOrderById, addModelOrder, removeModelOrder, editModelOrder) where

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

getOrder :: Int -> IO (Maybe OrderModel)
getOrder = S.get getParam
  where
    getParam :: Order -> IO (Maybe [Product], Customer) -- у заказа же всегда есть Customer? Или нужно сделать тоже Maybe Customer
    getParam ord = R.getCustomerByOrder ord >>= \maybeCustomer -> R.getProductsByOrder ord >>= \prods -> return (Just prods, maybeCustomer)

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

-- getModelOrderById :: Int -> IO (Maybe OrderModel)
-- getModelOrderById orderID =
--   let prods = getProductsByOrderId orderID
--       cust = getCustomerByOrderId orderID
--       ords = getEntityById orderID
--    in ords >>= \maybeOrd -> maybeOrd >>= \ord -> pure (mappingOrderToModel) ord <*> prods <*> cust

addModelOrder :: OrderModel -> IO Int
addModelOrder item = addEntity $ mappingModelToOrder item

removeModelOrder :: (GenericRepository a) => Int -> IO (Maybe a)
removeModelOrder = removeEid

editModelOrder :: OrderModel -> IO ()
editModelOrder item = editEntity $ mappingModelToOrder item
