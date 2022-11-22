module Repositories.OrderRepository
  ( getOrders,
    getOrderById,
    getOrdersByCustomer,
    getOrderByCustomerId,
  )
where

import Data.Context (orders)
import Data.Entities (Customer (..), Order (..))

getOrders :: [Order]
getOrders = orders

getOrderById :: Int -> Order
getOrderById searchId = head $ filter (\x -> orderId x == searchId) orders

getOrdersByCustomer :: Customer -> [Order]
getOrdersByCustomer (Customer sId _ _) = filter (\x -> orderId x == sId) orders

getOrderByCustomerId :: Int -> Order
getOrderByCustomerId sId = head $ filter (\x -> orderId x == sId) orders
