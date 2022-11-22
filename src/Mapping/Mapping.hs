module Mapping.Mapping
  ( mappingProductToModel,
    mappingShopToModel,
    mappingCustomerToModel,
    mappingOrderToModel,
  )
where

import Data.Entities
  ( Customer (customerAddress, customerId, customerName),
    Order (oNumber, orderId),
    Product,
    Shop (shopAddress, shopId, shopName),
    productColor,
    productId,
    productName,
    productPrice,
  )
import Data.Models
  ( CustomerModel (CustomerModel),
    OrderModel (OrderModel),
    ProductModel (..),
    ShopModel (ShopModel),
  )

mappingProductToModel :: Product -> Maybe Shop -> ProductModel
mappingProductToModel prod sh =
  let shopModel = case sh of
        Just value -> Just $ ShopModel (shopId value) (shopName value) (shopAddress value) Nothing
        Nothing -> Nothing
   in ProductModel (productId prod) shopModel (productName prod) (productPrice prod) (productColor prod)

mappingShopToModel :: Shop -> Maybe [Product] -> ShopModel
mappingShopToModel shop prod =
  let productModel = case prod of
        Just value -> Just $ productMapList value
        Nothing -> Nothing
   in ShopModel (shopId shop) (shopName shop) (shopAddress shop) productModel

mappingCustomerToModel :: Customer -> Maybe [Order] -> CustomerModel
mappingCustomerToModel cust ord =
  let orderModel = case ord of
        Just value -> Just $ orderMapList value
        Nothing -> Nothing
   in CustomerModel (customerId cust) (customerName cust) (customerAddress cust) orderModel

mappingOrderToModel :: Order -> Maybe [Product] -> Maybe Customer -> OrderModel
mappingOrderToModel ord prod cust =
  let productModel = case prod of
        Just value -> Just $ productMapList value
        Nothing -> Nothing
      customerModel = case cust of
        Just value -> Just $ CustomerModel (customerId value) (customerName value) (customerAddress value) (Nothing)
        Nothing -> Nothing
   in OrderModel (orderId ord) (customerModel) (oNumber ord) (productModel)

productMapList :: [Product] -> [ProductModel]
productMapList (x : xs) = (ProductModel (productId x) (Nothing) (productName x) (productPrice x) (productColor x)) : productMapList xs

orderMapList :: [Order] -> [OrderModel]
orderMapList (x : xs) = (OrderModel (orderId x) (Nothing) (oNumber x) Nothing) : orderMapList xs
