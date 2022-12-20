{-# LANGUAGE InstanceSigs #-}

module Data.Entities
  ( Product (..),
    Customer (..),
    Shop (..),
    Order (..),
    ProductOrder (..),
  )
where

import Data.CommonData (Color (..))

data Product = Product
  { productId :: Int,
    productShopId :: Int,
    productName :: String,
    productPrice :: String,
    productColor :: Color
  }

instance Eq Product where
  (==) :: Product -> Product -> Bool
  x == y = productId x == productId y

instance Show Product where
  show (Product _ prodShopId name price clr) = "(" ++ show prodShopId ++ ", " ++ toSqlQuotes name ++ "," ++ show price ++ ", " ++ toSqlQuotes (show clr) ++ ")"

data Customer = Customer
  { customerId :: Int,
    customerName :: String,
    customerAddress :: String
  }

instance Show Customer where
  show (Customer _ custName custAdd) = "(" ++ toSqlQuotes custName ++ "," ++ toSqlQuotes custAdd ++ ")"

instance Eq Customer where
  (==) :: Customer -> Customer -> Bool
  x == y = customerId x == customerId y

data Shop = Shop
  { shopId :: Int,
    shopName :: String,
    shopAddress :: String
  }

instance Show Shop where
  show (Shop _ name address) = "(" ++ toSqlQuotes name ++ "," ++ toSqlQuotes address ++ ")"

instance Eq Shop where
  (==) :: Shop -> Shop -> Bool
  x == y = shopId x == shopId y

data Order = Order
  { orderId :: Int,
    oCId :: Int,
    oNumber :: String
  }

instance Eq Order where
  (==) :: Order -> Order -> Bool
  x == y = orderId x == orderId y

instance Show Order where
  show (Order _ ocid onum) = "(" ++ show ocid ++ "," ++ toSqlQuotes onum ++ ")"

data ProductOrder = ProductOrder
  { productOrderId :: Int,
    poId :: Int,
    opId :: Int
  }

instance Show ProductOrder where
  show (ProductOrder _ pId oId) = "(" ++ show pId ++ "," ++ show oId ++ ")"

instance Eq ProductOrder where
  (==) :: ProductOrder -> ProductOrder -> Bool
  x == y = productOrderId x == productOrderId y

toSqlQuotes :: String -> String
toSqlQuotes str = "'" ++ str ++ "'"