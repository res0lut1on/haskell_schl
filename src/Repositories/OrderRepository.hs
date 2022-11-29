module Repositories.OrderRepository
  ( getOrd,
    getOrderById,
    getOrderByCustomerId,
    getOrdersByCustomerId,
    updateOrder,
    addOrder,
    removeOrder,
  )
where

import Data.Entities (Order (..))
import Util.Utilities (maybeHead)
import Util.FileUtil

getOrd :: IO [Order]
getOrd = readAllOrders

getOrderById :: Int -> IO (Maybe Order) 
getOrderById searchId = maybeHead . filter (\x -> orderId x == searchId) <$> getOrd

getOrderByCustomerId :: Int -> IO (Maybe Order)
getOrderByCustomerId sId = maybeHead . filter (\x -> oCId x == sId) <$> getOrd

getOrdersByCustomerId :: Int -> IO [Order]
getOrdersByCustomerId sId = filter (\x -> oCId x == sId) <$> getOrd

addOrder :: Order -> IO Int
addOrder item =
  do
    allItem <- readAllOrders
    let prevItem = head allItem
    let iId = orderId prevItem + 1
    addOrd 
      ( Order
          { orderId = iId,
            oCId = oCId item,
            oNumber = oNumber item
          }
      )
    return iId

removeOrder :: Int -> IO ()
removeOrder = removeOrd

updateOrder :: Order -> IO ()
updateOrder = updateOrd

