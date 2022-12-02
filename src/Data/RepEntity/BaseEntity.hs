{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Data.RepEntity.BaseEntity (BaseEntity (..),EntityName) where

import Data.Entities

data EntityType = Products | Shops | Customers | Orders | ProductToOrders
  deriving (Read, Show)

class BaseEntity a where
  entType :: a -> a
  entName :: a -> String
  entId :: a -> Int
  changeId :: a -> Int -> a
  entityName :: EntityName a

data EntityName a = EntityName
  { returnNameEntity :: String
  }

instance BaseEntity Customer where
  entName :: a -> String
  entName _ = "customers"
  entType :: Customer -> Customer
  entType s = s
  entId :: Customer -> Int
  entId (Customer cId _ _) = cId
  changeId :: Customer -> Int -> Customer
  changeId cust newId = cust {customerId = newId}
  entityName :: EntityName Customer
  entityName = EntityName $ show Customers

instance BaseEntity Shop where
  entName :: a -> String
  entName _ = "shops"
  entType :: Shop -> Shop
  entType s = s
  entId :: Shop -> Int
  entId (Shop sId _ _) = sId
  changeId :: Shop -> Int -> Shop
  changeId shp newId = shp {shopId = newId}

instance BaseEntity Product where
  entName :: a -> String
  entName _ = "products"
  entType :: Product -> Product
  entType s = s
  entId :: Product -> Int
  entId (Product pId _ _ _ _) = pId
  changeId :: Product -> Int -> Product
  changeId prd newId = Product newId (productShopId prd) (productName prd) (productPrice prd) (productColor prd)

instance BaseEntity Order where
  entName :: a -> String
  entName _ = "orders"
  entType :: Order -> Order
  entType s = s
  entId :: Order -> Int
  entId (Order oId _ _) = oId
  changeId :: Order -> Int -> Order
  changeId ord newId = ord {orderId = newId}

instance BaseEntity ProductOrder where
  entName :: a -> String
  entName _ = "productOrder"
  entType :: ProductOrder -> ProductOrder
  entType s = s
  entId :: ProductOrder -> Int
  entId (ProductOrder oId _ _) = oId
  changeId :: ProductOrder -> Int -> ProductOrder
  changeId pd newId = pd {poId = newId}