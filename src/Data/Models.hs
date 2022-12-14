module Data.Models
  ( ShopModel (..),
    OrderModel (..),
    ProductModel (..),
    CustomerModel (..),
    productModelId,
    productModelShop,
    productModelName,
    productModelPrice,
    productModelColor,
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


