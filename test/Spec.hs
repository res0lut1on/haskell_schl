module Spec
  ( someTest,
  )
where

import Repositories.CustomersRepository
import Repositories.OrderRepository
import Repositories.ShopRepository
import Repositories.ProductRepository

someTest :: IO ()
someTest = do
  putStrLn ""
  putStrLn "All Customers"
  putStrLn ""
  print (getCustomers)
  putStrLn ""
  putStrLn "Customer, id =2 "
  putStrLn ""
  print (getCustomerById 2)
  putStrLn ""
  putStrLn "All Product"
  putStrLn ""
  print (getProducts)
  putStrLn ""
  putStrLn "All Orders"
  putStrLn ""
  print (getOrders)
  putStrLn ""
  putStrLn "Order List for Customer id = 3"
  putStrLn ""
  print (getOrdersByCustomerId 3)
  putStrLn ""
  putStrLn "Product List for Order id = 3"
  putStrLn ""
  print (getProductsByOrderId 3)
  putStrLn ""



