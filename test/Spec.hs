module Spec
  ( busTest,
    foldTest,
  )
where

import LibFold

import Repositories.CustomersRepository
import Repositories.OrderRepository
import Repositories.ProductRepository
import Repositories.ShopRepository

import Data.Models

import Services.CustomerServices

import Data.Entities
import Util.FileUtil

busTest :: IO ()
busTest = do
  out <- initDataBase
  putStrLn ""
  putStrLn "All Customers"
  putStrLn ""
  out <- getCustomers
  print (out)
  putStrLn ""
  putStrLn "Customer, id = 2 "
  putStrLn ""
  out <- getCustomerById 2
  print (out)
  putStrLn ""
  putStrLn "All Product"
  putStrLn ""
  out <- getProducts
  print (out)
  putStrLn ""
  putStrLn "Product, id = 3"
  putStrLn ""
  out <- getProductById 3
  print (out)
  putStrLn ""
  putStrLn "All Shops"
  putStrLn ""
  out <- getShops
  print (out)
  putStrLn ""
  putStrLn "Shop, id = 3"
  putStrLn ""
  out <- getShopById 1
  print (out)
  putStrLn ""
  putStrLn "All Orders"
  putStrLn ""
  out <- getOrd
  print (out)
  putStrLn ""
  putStrLn "Order, id = 2"
  putStrLn ""
  out <- getOrderById 2
  print (out)
  putStrLn ""
  putStrLn "Delete Customer, id = 2"
  removeCustomer 2      
  out <- readAllCustomers
  print out
  putStrLn ""
  putStrLn "Add Customer"
  addCustomer (Customer{customerName = "Bob", customerId = 2, customerAddress = "Brugge"})
  putStrLn ""
  out <- readAllCustomers
  putStrLn ""
  print out
  initDataBase
  putStrLn ""
  putStrLn "All Model Customer"
  putStrLn ""
  out <- getModelCustomers
  print out
  putStrLn ""
  putStrLn "Add Model Customer"
  putStrLn ""
  addModelCustomer (CustomerModel {
    customerModelId = 43,
    customerModelAddress = "Kochka",
    customerModelName = "Killreal",
    customerModelOrders = Nothing
  })
  out <- getModelCustomers
  print out 
  out <- getCustomers
  print out 
   
  putStrLn ""
  
-- data OrderModel = OrderModel
--   { orderModelId :: Int,
--     orderModelCustomer :: Maybe CustomerModel,
--     orderModelNumber :: String,
--     orderProducts :: Maybe [ProductModel]
--   }

  -- putStrLn "Order List for Customer id = 3"
  -- putStrLn ""
  -- print (getOrdersByCustomerId 3)
  -- putStrLn ""
  -- putStrLn "Product List for Order id = 3"
  -- putStrLn ""
  -- print (getProductsByOrderId 4)
  -- putStrLn ""
  -- putStrLn ""
  -- print (sum [1,2,3,4])
  -- putStrLn ""


