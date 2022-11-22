module Repositories.OrderRepository
  ( getOrders,
    getOrderById,
    getOrdersByCustomer,
    getOrdersByCustomerId,
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

getOrdersByCustomerId :: Int -> Order
getOrdersByCustomerId sId = head $ filter (\x -> orderId x == sId) orders
