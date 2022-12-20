{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Data.RepEntity.BaseEntity (BaseEntity (..), EntityName (..)) where

import Data.Entities

data EntityType = Products | Shops | Customers | Orders
  deriving (Read, Show)

class BaseEntity a where
  entType :: a -> a
  entName :: a -> String
  entId :: a -> Int
  entIdField :: a -> String
  changeId :: a -> Int -> a
  entityName :: EntityName a
  entityField :: a -> String

data EntityName a = EntityName
  { returnNameEntity :: String
  }

instance BaseEntity Customer where
  entName :: a -> String
  entName _ = "Customer"
  entType :: Customer -> Customer
  entType s = s
  entId :: Customer -> Int
  entId (Customer cId _ _) = cId
  entIdField :: a -> String
  entIdField _ = "CustomerID"
  changeId :: Customer -> Int -> Customer
  changeId cust newId = cust {customerId = newId}
  entityName :: EntityName Customer
  entityName = EntityName $ show Customers
  entityField :: a -> String
  entityField _ = "(CustomerName, CustomerAddress)"

instance BaseEntity Shop where
  entName :: a -> String
  entName _ = "Shop"
  entType :: Shop -> Shop
  entType s = s
  entId :: Shop -> Int
  entId (Shop sId _ _) = sId
  entIdField :: a -> String
  entIdField _ = "ShopID"
  changeId :: Shop -> Int -> Shop
  changeId shp newId = shp {shopId = newId}
  entityName :: EntityName Shop
  entityName = EntityName $ show Shops
  entityField :: a -> String
  entityField _ = "(ShopName, ShopAddress)"

instance BaseEntity Product where
  entName :: a -> String
  entName _ = "Product"
  entType :: Product -> Product
  entType s = s
  entId :: Product -> Int
  entId (Product pId _ _ _ _) = pId
  entIdField :: a -> String
  entIdField _ = "ProductID"
  changeId :: Product -> Int -> Product
  changeId prd newId = Product newId (productShopId prd) (productName prd) (productPrice prd) (productColor prd)
  entityName :: EntityName Product
  entityName = EntityName $ show Products
  entityField :: a -> String
  entityField _ = "(ShopID, ProductName, Price, ColorID)"

instance BaseEntity Order where
  entName :: a -> String
  entName _ = "Order"
  entType :: Order -> Order
  entType s = s
  entId :: Order -> Int
  entId (Order oId _ _) = oId
  entIdField :: a -> String
  entIdField _ = "OrderID"
  changeId :: Order -> Int -> Order
  changeId ord newId = ord {orderId = newId}
  entityName :: EntityName Order
  entityName = EntityName $ show Orders
  entityField :: a -> String
  entityField _ = "(CustomerID, OrderNumber)"

instance BaseEntity ProductOrder where
  entName :: a -> String
  entName _ = "ProductOrder"
  entType :: ProductOrder -> ProductOrder
  entType s = s
  entId :: ProductOrder -> Int
  entId (ProductOrder oId _ _) = oId
  entIdField :: a -> String
  entIdField _ = "ProductOrderID"
  changeId :: ProductOrder -> Int -> ProductOrder
  changeId pd newId = pd {poId = newId}
  entityName :: EntityName ProductOrder
  entityName = EntityName "ProductOrder"
  entityField :: a -> String
  entityField _ = "(ProductID, OrderID)"