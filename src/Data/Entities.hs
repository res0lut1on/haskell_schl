{-# LANGUAGE InstanceSigs #-}

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

instance Eq Product where
  (==) :: Product -> Product -> Bool
  x == y = productId x == productId y

data Customer = Customer
  { customerId :: Int,
    customerName :: String,
    customerAddress :: String
  }
  deriving (Show)

instance Eq Customer where
  (==) :: Customer -> Customer -> Bool
  x == y = customerId x == customerId y

data Shop = Shop
  { shopId :: Int,
    shopName :: String,
    shopAddress :: String
  }
  deriving (Show)

instance Eq Shop where
  (==) :: Shop -> Shop -> Bool
  x == y = shopId x == shopId y

data Order = Order
  { orderId :: Int,
    oCId :: Int,
    oNumber :: String
  }
  deriving (Show)

instance Eq Order where
  (==) :: Order -> Order -> Bool
  x == y = orderId x == orderId y

data ProductOrder = ProductOrder
  { productOrderId :: Int,
    poId :: Int,
    opId :: Int
  }
  deriving (Show)

instance Eq ProductOrder  where
  (==) :: ProductOrder -> ProductOrder -> Bool
  x == y = productOrderId x == productOrderId y