{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Services.SearchService (SearchService (..)) where

import Data.Entities (Customer (..), Order (..), Product, Shop (..), productColor, productName, productPrice)
import Data.List (isInfixOf)
import Data.RepEntity.BaseEntity (BaseEntity)
import Data.SearchModel (CustomerSearchModel (..), OrderSearchModel (..), ProductSearchModel (..), SearchModel, ShopSearchModel (shopSearchModelAddress, shopSearchModelName))
import Services.ApplyFilter (applyFilter)

class (SearchModel a, BaseEntity b) => SearchService a b where
  searchModel :: a -> [b] -> [b]

instance SearchService ProductSearchModel Product where
  searchModel :: ProductSearchModel -> [Product] -> [Product]
  searchModel filters =
    applyFilter productName productSearchModelName isInfixOf filters
      . applyFilter productPrice productSearchModelPrice (==) filters
      . applyFilter productColor productSearchModelColor (==) filters

instance SearchService OrderSearchModel Order where
  searchModel :: OrderSearchModel -> [Order] -> [Order]
  searchModel = applyFilter oNumber orderSearchModelNumber isInfixOf

instance SearchService ShopSearchModel Shop where
  searchModel :: ShopSearchModel -> [Shop] -> [Shop]
  searchModel filters =
    applyFilter shopName shopSearchModelName isInfixOf filters
      . applyFilter shopAddress shopSearchModelAddress isInfixOf filters

instance SearchService CustomerSearchModel Customer where
  searchModel :: CustomerSearchModel -> [Customer] -> [Customer]
  searchModel filters =
    applyFilter customerName customerSearchModelName isInfixOf filters
      . applyFilter customerAddress customerSearchModelAddress isInfixOf filters