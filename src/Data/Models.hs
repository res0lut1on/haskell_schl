module Data.Models
  ( ShopModel (..),
    OrderModel (..),
    ProductModel (..),
    CustomerModel (..),
  )
where

import Data.CommonData (Color)

data ProductModel = ProductModel
  { productModelId :: Int,
    productModelShop :: Maybe ShopModel,
    productModelName :: String,
    productModelPrice :: Double,
    productModelColor :: Color
  }

instance Show ProductModel where
  show (ProductModel item1 item2 item3 item4 _) = show item1 ++ ", " ++ show item2 ++ ", " ++ item3 ++ ", " ++ show item4 ++ "\n"

data ShopModel = ShopModel
  { shopModelId :: Int,
    shopModelName :: String,
    shopModelAddress :: String,
    shopModelProducts :: Maybe [ProductModel]
  }

instance Show ShopModel where
  show (ShopModel item1 item2 item3 _) = show item1 ++ ", " ++ item2 ++ ", " ++ item3

data OrderModel = OrderModel
  { orderModelId :: Int,
    orderModelCustomer :: Maybe CustomerModel,
    orderModelNumber :: String,
    orderProducts :: Maybe [ProductModel]
  }

instance Show OrderModel where
  show (OrderModel item1 item2 item3 item4) = show item1 ++ ", " ++ show item2 ++ ", " ++ item3 ++ ", " ++ show item4

data CustomerModel = CustomerModel
  { customerModelId :: Int,
    customerModelName :: String,
    customerModelAddress :: String,
    customerModelOrders :: Maybe [OrderModel]
  }

instance Show CustomerModel where
  show (CustomerModel item1 item2 item3 _) = "\n" ++ show item1 ++ ", " ++ item2 ++ ", " ++ item3
