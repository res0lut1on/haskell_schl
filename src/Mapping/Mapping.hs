module Mapping.Mapping
  ( mappingProductToModel,
    mappingShopToModel,
    mappingCustomerToModel,
    mappingOrderToModel,
    mappingModelToProduct,
    mappingModelToShop,
    mappingModelToCustomer,
    mappingModelToOrder,
    mappingModelToProductOrder,
  )
where

import Data.Entities
  ( Customer (Customer, customerAddress, customerId, customerName),
    Order (..),
    Product (Product),
    ProductOrder (..),
    Shop (..),
    productColor,
    productId,
    productName,
    productPrice,
  )
import Data.Maybe (fromMaybe)
import Data.Models
  ( CustomerModel (..),
    OrderModel (OrderModel, orderModelCustomer, orderModelId, orderModelNumber, orderProducts),
    ProductModel (..),
    ShopModel (..),
    productModelColor,
    productModelId,
    productModelName,
    productModelPrice,
    productModelShop,
  )
import Util.Utilities

mappingProductToModel :: Product -> Maybe Shop -> ProductModel -- !
mappingProductToModel prod sh =
  let shopModel = case sh of
        Just value -> Just $ mappingShopToModel value Nothing
        Nothing -> Nothing
   in ProductModel (productId prod) shopModel (productName prod) (productPrice prod) (productColor prod)

mappingModelToProduct :: ProductModel -> Product
mappingModelToProduct modelProduct =
  let shId = shopModelId <$> productModelShop modelProduct
   in Product (productModelId modelProduct) (fromMaybe (-1) shId) (productModelName modelProduct) (productModelPrice modelProduct) (productModelColor modelProduct)

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

mappingModelToShop :: ShopModel -> Shop
mappingModelToShop modelShop =
  Shop
    { shopId = shopModelId modelShop,
      shopName = shopModelName modelShop,
      shopAddress = shopModelAddress modelShop
    }

mappingCustomerToModel :: Customer -> Maybe [Order] -> Maybe [(Int, [Product])] -> CustomerModel
mappingCustomerToModel cust ord prodDict =
  let orderModel = case ord of
        Just orderV -> case prodDict of
          Just prods -> Just $ map (\x -> mappingOrderToModel x (getProductsFromDict x prods) Nothing) orderV
          Nothing -> Just $ map (\o -> mappingOrderToModel o Nothing Nothing) orderV
        Nothing -> Nothing
   in CustomerModel
        { customerModelId = customerId cust,
          customerModelName = customerName cust,
          customerModelAddress = customerAddress cust,
          customerModelOrders = orderModel
        }
  where
    getProductsFromDict order = maybeHead . map snd . filter (\x -> fst x == orderId order)

mappingOrderToModel :: Order -> Maybe [Product] -> Maybe Customer -> OrderModel
mappingOrderToModel ord prod cust =
  let productModel = case prod of
        Just value -> Just $ map (`mappingProductToModel` Nothing) value
        Nothing -> Nothing
      customerModel = case cust of
        Just value -> Just $ mappingCustomerToModel value Nothing Nothing
        Nothing -> Nothing
   in OrderModel
        { orderModelId = orderId ord,
          orderModelNumber = oNumber ord,
          orderModelCustomer = customerModel,
          orderProducts = productModel
        }

mappingModelToCustomer :: CustomerModel -> Customer
mappingModelToCustomer modelCustomer =
  Customer
    { customerId = customerModelId modelCustomer,
      customerName = customerModelName modelCustomer,
      customerAddress = customerModelAddress modelCustomer
    }

mappingModelToOrder :: OrderModel -> Order
mappingModelToOrder modelOrder =
  let ordCustId = customerModelId <$> orderModelCustomer modelOrder
   in Order
        { orderId = orderModelId modelOrder,
          oCId = fromMaybe (-1) ordCustId,
          oNumber = orderModelNumber modelOrder
        }

mappingModelToProductOrder :: Int -> Int -> ProductModel -> ProductOrder
mappingModelToProductOrder ordId proOrdId modelProduct =
  let prId = productModelId modelProduct
   in ProductOrder
        { poId = prId,
          opId = ordId,
          productOrderId = proOrdId
        }
