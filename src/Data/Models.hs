module Data.Models
  ( ShopModel (..),
    OrderModel (..),
    ProductModel (..),
    CustomerModel (..),
  )
where

import Data.CommonData (Color)

data ProductModel = ProductModel Int (Maybe ShopModel) String Double Color

productModelId :: ProductModel -> Int
productModelId (ProductModel pId _ _ _ _) = pId

productModelShop :: ProductModel -> Maybe ShopModel
productModelShop (ProductModel _ ps _ _ _) = ps

productModelName :: ProductModel -> String
productModelName (ProductModel _ _ name _ _) = name

productModelPrice :: ProductModel -> Double
productModelPrice (ProductModel _ _ _ pmp _) = pmp

productModelColor :: ProductModel -> Color
productModelColor (ProductModel _ _ _ _ color) = color

data ShopModel = ShopModel
  { shopModelId :: Int,
    shopModelName :: String,
    shopModelAddress :: String,
    shopModelProducts :: Maybe [ProductModel]
  }

data OrderModel = OrderModel
  { orderModelId :: Int,
    orderModelCustomer :: Maybe CustomerModel,
    orderModelNumber :: String,
    orderProducts :: Maybe [ProductModel]
  }

data CustomerModel = CustomerModel
  { customerModelId :: Int,
    customerModelName :: String,
    customerModelAddress :: String,
    customerModelOrders :: Maybe [OrderModel]
  }
