module Mapping.Mapping
  ( mappingProductToModel,
    mappingShopToModel,
  )
where

import Data.Entities
import Data.Models

mappingProductToModel :: Product -> Maybe Shop -> ProductModel
mappingProductToModel prod sh =
  let shopModel = case sh of
        Just value -> Just $ ShopModel (shopId value) (shopName value) (shopAddress value) Nothing
        Nothing -> Nothing
   in ProductModel (productId prod) shopModel (productName prod) (productPrice prod) (productColor prod)

mappingShopToModel :: Shop -> Maybe [Product] -> ShopModel
mappingShopToModel shop prod =
  let products = case prod of
        Just value -> Just $ ProductModel (productId prod) (shopName value) (shopAddress value) Nothing
        Nothing -> Nothing
   in ShopModel (shopId shop) (shopName shop) (shopAddress shop) products

newMapList :: [Product] -> [ProductModel]
newMapList s = newMapList'  