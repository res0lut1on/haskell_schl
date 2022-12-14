module Spec
  ( busTest,
    foldTest,
  )
where

import Data.CommonData (Color (White))
import Data.Models
import Data.SearchModel
import LibFold
import Services.EntityServices
import Services.SearchService ()
import Util.FileUtil
import Services.ProductServices (getProduct)

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
  getProduct >>= print
exampleShopSearchModel = ShopSearchModel {shopSearchModelName = Just "Green", shopSearchModelPage = 1, shopSearchModelAddress = Nothing}

exampleProductSearchModel = ProductSearchModel Nothing Nothing (Just White) 1