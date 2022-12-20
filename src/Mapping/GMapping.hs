{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mapping.GMapping (GenericMapping (..)) where

import Data.Entities (Customer (..), Order (..), Product (..), Shop (..), productColor, productId, productName, productPrice)
import Data.Maybe (fromMaybe)
import Data.Models (CustomerModel (..), OrderModel (..), ProductModel (ProductModel), ShopModel (..), productModelColor, productModelId, productModelName, productModelPrice, productModelShop)

class GenericMapping a b where
  toModel :: a -> b

  toList :: [a] -> [b]
  toList = map toModel

instance GenericMapping ProductModel Product where
  toModel :: ProductModel -> Product
  toModel pM =
    let shpId = shopModelId <$> productModelShop pM
     in Product
          { productId = productModelId pM,
            productShopId = fromMaybe (-1) shpId,
            productName = productModelName pM,
            productPrice = productModelPrice pM,
            productColor = productModelColor pM
          }

instance GenericMapping Product ProductModel where
  toModel :: Product -> ProductModel
  toModel prod =
    ProductModel
      { productModelId = productId prod,
        productModelShop = Nothing,
        productModelName = productName prod,
        productModelPrice = productPrice prod,
        productModelColor = productColor prod
      }

instance GenericMapping Order OrderModel where
  toModel :: Order -> OrderModel
  toModel ord =
    OrderModel
      { orderModelId = orderId ord,
        orderModelNumber = oNumber ord,
        orderModelCustomer = Nothing,
        orderProducts = Nothing
      }

instance GenericMapping OrderModel Order where
  toModel :: OrderModel -> Order
  toModel oM =
    let oMcustomer = customerModelId <$> orderModelCustomer oM
     in Order
          { orderId = orderModelId oM,
            oNumber = orderModelNumber oM,
            oCId = fromMaybe (-1) oMcustomer
          }

instance GenericMapping Shop ShopModel where
  toModel :: Shop -> ShopModel
  toModel shp =
    ShopModel
      { shopModelId = shopId shp,
        shopModelAddress = shopAddress shp,
        shopModelName = shopName shp,
        shopModelProducts = Nothing
      }

instance GenericMapping ShopModel Shop where
  toModel :: ShopModel -> Shop
  toModel shpM =
    Shop
      { shopId = shopModelId shpM,
        shopAddress = shopModelAddress shpM,
        shopName = shopModelName shpM
      }

instance GenericMapping Customer CustomerModel where
  toModel :: Customer -> CustomerModel
  toModel cust =
    CustomerModel
      { customerModelId = customerId cust,
        customerModelAddress = customerAddress cust,
        customerModelOrders = Nothing,
        customerModelName = customerName cust
      }

instance GenericMapping CustomerModel Customer where
  toModel :: CustomerModel -> Customer
  toModel custM =
    Customer
      { customerId = customerModelId custM,
        customerName = customerModelName custM,
        customerAddress = customerModelAddress custM
      }
