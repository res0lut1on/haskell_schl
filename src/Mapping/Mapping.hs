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
  ( CustomerModel (..),
    OrderModel (OrderModel, orderModelCustomer, orderModelId, orderModelNumber, orderProducts),
    ProductModel (..),
    ShopModel (..),
  )
import Repositories.ProductRepository (getProductsByOrderId)

mappingProductToModel :: Product -> Maybe Shop -> ProductModel -- !
mappingProductToModel prod sh =
  let shopModel = case sh of
        Just value -> Just $ mappingShopToModel value Nothing
        Nothing -> Nothing
   in ProductModel (productId prod) shopModel (productName prod) (productPrice prod) (productColor prod)

mappingShopToModel :: Shop -> Maybe [Product] -> ShopModel
mappingShopToModel shop prod =
  let productModel = case prod of
        Just value -> Just $ map (`mappingProductToModel` Nothing) value
        Nothing -> Nothing
   in ShopModel
        { shopModelId = shopId shop,
          shopModelName = shopName shop,
          shopModelAddress = shopAddress shop,
          shopModelProducts = productModel
        }

mappingCustomerToModel :: Customer -> Maybe [Order] -> CustomerModel
mappingCustomerToModel cust ord =
  let orderModel = case ord of
        Just value -> Just $ map (\x -> mappingOrderToModel x (Just $ getProductsByOrderId (orderId x)) Nothing) value -- стоит переделать без прямой связи с Repository?
        Nothing -> Nothing
   in CustomerModel
        { customerModelId = customerId cust,
          customerModelName = customerName cust,
          customerModelAddress = customerAddress cust,
          customerModelOrders = orderModel
        }

mappingOrderToModel :: Order -> Maybe [Product] -> Maybe Customer -> OrderModel
mappingOrderToModel ord prod cust =
  let productModel = case prod of
        Just value -> Just $ map (`mappingProductToModel` Nothing) value
        Nothing -> Nothing
      customerModel = case cust of
        Just value -> Just $ mappingCustomerToModel value Nothing
        Nothing -> Nothing
   in OrderModel
        { orderModelId = orderId ord,
          orderModelNumber = oNumber ord,
          orderModelCustomer = customerModel,
          orderProducts = productModel
        }
