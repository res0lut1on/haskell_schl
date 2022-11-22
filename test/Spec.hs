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
  putStrLn "Products List for Shops id = 2"
  putStrLn ""
  print (getProductsByShop (getShopById 2))
  putStrLn ""
  putStrLn "All Orders"
  putStrLn ""
  print (getOrders)
  putStrLn ""
  putStrLn "Order List for Customer id = 3"
  putStrLn ""
  print (getOrdersByCustomer(getCustomerById 3))
  putStrLn ""



