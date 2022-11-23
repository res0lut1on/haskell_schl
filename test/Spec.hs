module Spec
  ( busTest,
    foldTest,
  )
where

import Repositories.CustomersRepository
import Repositories.OrderRepository
import Repositories.ProductRepository
import Repositories.ShopRepository

busTest :: IO ()
busTest = do
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
  print (getProductsByOrderId 4)
  putStrLn ""

foldTest :: IO ()
foldTest = do
  putStrLn ""
  putStrLn "foldl (/) 27 [3,1,3]"
  putStrLn ""
  print (foldl (/) 27 [3, 1, 3])
  putStrLn ""
  putStrLn "foldr (*) 1 [3,3,3]"
  putStrLn ""
  print (foldr (*) 1 [3, 3, 3])
  putStrLn ""
  putStrLn "fold' (+) 0 [1,2,3]"
  putStrLn ""
  print (foldr (+) 0 [1, 2, 3])
  putStrLn ""
  putStr "map (*2) [4,3,2,1,5] = "
  putStrLn ""
  print (show (map (* 2) [4, 3, 2, 1, 5]))
  putStrLn ""
