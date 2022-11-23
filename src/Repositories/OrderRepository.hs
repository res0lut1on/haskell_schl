module Repositories.OrderRepository
  ( getOrders,
    getOrderById,
    getOrderByCustomerId,
    getOrdersByCustomerId,
  )
where

import Data.Context (orders)
import Data.Entities (Customer (..), Order (..))
import Utilities (maybeHead)

getOrders :: [Order]
getOrders = orders

getOrderById :: Int -> Maybe Order
getOrderById searchId = maybeHead $ filter (\x -> orderId x == searchId) getOrders

getOrderByCustomerId :: Int -> Maybe Order
getOrderByCustomerId sId = maybeHead $ filter (\x -> oCId x == sId) getOrders

getOrdersByCustomerId :: Int -> [Order]
getOrdersByCustomerId sId = filter (\x -> oCId x == sId) getOrders