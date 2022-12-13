{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mapping.MappingParam (MappingParam (..)) where

import Data.Entities
  ( Customer,
    Order (..),
    Product,
    Shop,
    productColor,
    productId,
    productName,
    productPrice,
  )
import Data.Models (CustomerModel (..), OrderModel (..), ProductModel (ProductModel), ShopModel (..))
import Mapping.GMapping (GenericMapping (..))
import Util.Utilities

class (GenericMapping a b) => MappingParam a b c where
  toModelParam :: a -> c -> b

instance MappingParam Order OrderModel (Maybe [Product], Customer) where
  toModelParam :: Order -> (Maybe [Product], Customer) -> OrderModel
  toModelParam ord (maybeProds, custs) =
    OrderModel
      { orderModelId = orderId ord,
        orderModelCustomer = Just $ toModel custs,
        orderModelNumber = oNumber ord,
        orderProducts = toList <$> maybeProds
      }

instance MappingParam Product ProductModel (Maybe Shop) where
  toModelParam :: Product -> Maybe Shop -> ProductModel
  toModelParam prod maybeShp = ProductModel (productId prod) (toModel <$> maybeShp) (productName prod) (productPrice prod) (productColor prod)

instance MappingParam Customer CustomerModel (Maybe [Order], Maybe [(Int, [Product])]) where
  toModelParam :: Customer -> (Maybe [Order], Maybe [(Int, [Product])]) -> CustomerModel
  toModelParam cust (maybeOrds, maybeProdsWithOrderId) =
    let ordsWithProducts = maybeOrds >>= \ords -> maybeProdsWithOrderId >>= \prodsWithOrderId -> return $ orderModelWithProducts prodsWithOrderId ords
     in (toModel cust) {customerModelOrders = ordsWithProducts}
    where
      getProductsFromDict :: Order -> [(Int, [Product])] -> Maybe [Product]
      getProductsFromDict ord = maybeHead . map snd . filter (\x -> fst x == orderId ord)

      orderModelWithProducts :: [(Int, [Product])] -> [Order] -> [OrderModel]
      orderModelWithProducts dict = map (\x -> toModelParam x (getProductsFromDict x dict, cust))

instance MappingParam Shop ShopModel (Maybe [Product]) where
  toModelParam :: Shop -> Maybe [Product] -> ShopModel
  toModelParam shp prods = (toModel shp) {shopModelProducts = toList <$> prods}