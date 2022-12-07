module Spec
  ( busTest,
    foldTest,
  )
where

import Data.Entities
import Data.Models
import LibFold
import Repositories.GenericRepository
import Services.EntityServices
import Util.FileUtil

busTest :: IO ()
busTest = do
  out <- initDataBase
  putStrLn ""
  putStrLn "All Customers"
  putStrLn ""
  out <- getModelCustomers
  print (out)
  putStrLn ""
  putStrLn "Customer, id = 2 "
  putStrLn ""
  out <- getModelCustomerById 2
  print (out)
  putStrLn ""
  putStrLn "All Product"
  putStrLn ""
  out <- getModelProducts
  print (out)
  putStrLn ""
  putStrLn "Product, id = 3"
  putStrLn ""
  out <- getModelProductById 3
  print (out)
  putStrLn ""
  putStrLn "All Shops"
  putStrLn ""
  out <- getModelShops
  print (out)
  putStrLn ""
  putStrLn "Shop, id = 3"
  putStrLn ""
  out <- getModelShopById 1
  print (out)
  putStrLn ""
  putStrLn "All Orders"
  putStrLn ""
  out <- getOrders
  print (out)
  putStrLn ""
  putStrLn "Order, id = 3"
  putStrLn ""
  out <- getModelOrderById 3
  print (out)
  putStrLn ""
  putStrLn "Delete Customer, id = 2"
  removeModelCustomer 2
  out <- getModelCustomers
  print out
  putStrLn ""
  putStrLn "Add Customer"
  out <- addModelCustomer (CustomerModel {customerModelName = "Bob", customerModelId = 2, customerModelAddress = "Brugge", customerModelOrders = Nothing})
  print out
  putStrLn ""
  out <- getModelCustomers
  putStrLn ""
  print out
  initDataBase
  putStrLn ""
