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

type ProductId = Int

type ProductShopId = Int

type ProductName = String

type ProductPrice = Double

data Product = Product ProductId ProductShopId ProductName ProductPrice Color
  deriving (Show)

productId :: Product -> ProductId
productId (Product prodId _ _ _ _) = prodId

productShopId :: Product -> ProductShopId
productShopId (Product _ pshopId _ _ _) = pshopId

productName :: Product -> ProductName
productName (Product _ _ name _ _) = name

productPrice :: Product -> ProductPrice
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
  deriving (Show)