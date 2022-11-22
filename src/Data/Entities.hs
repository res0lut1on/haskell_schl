module Data.Entities
  ( Product (..),
    Customer (..),
    Shop (..),
    Order (..),
    ProductOrder (..),
    productId,
    productShopId,
    productName,
    productPrice,
    productColor,
  )
where

import Data.CommonData (Color (..))

data Product = Product Int Int String Double Color
  deriving (Show)

productId :: Product -> Int
productId (Product prodId _ _ _ _) = prodId

productShopId :: Product -> Int
productShopId (Product _ pshopId _ _ _) = pshopId

productName :: Product -> String
productName (Product _ _ name _ _) = name

productPrice :: Product -> Double
productPrice (Product _ _ _ price _) = price

productColor :: Product -> Color
productColor (Product _ _ _ _ color) = color

data Customer = Customer
  { customerId :: Int,
    customerName :: String,
    customerAddress :: String
  }
  deriving (Show)

data Shop = Shop
  { shopId :: Int,
    shopName :: String,
    shopAddress :: String
  }
  deriving (Show)

data Order = Order
  { orderId :: Int,
    oCId :: Int,
    oNumber :: String
  }
  deriving (Show)

data ProductOrder = ProductOrder
  { productOrderId :: Int,
    poId :: Int,
    opId :: Int
  }