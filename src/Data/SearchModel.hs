module Data.SearchModel (ProductSearchModel (..), ShopSearchModel (..), CustomerSearchModel (..), OrderSearchModel (..), SearchModel (..)) where

import Data.CommonData (Color)

class SearchModel a where
  getPageNumber :: a -> Int
  getPageSize :: a -> Int
  getPageSize _ = 4 ----------------------------------------------

instance SearchModel ProductSearchModel where
  getPageNumber = productSearchModelPage

instance SearchModel ShopSearchModel where
  getPageNumber = shopSearchModelPage

instance SearchModel OrderSearchModel where
  getPageNumber = orderSearchModelPage

instance SearchModel CustomerSearchModel where
  getPageNumber = customerSearchModelPage

data ProductSearchModel = ProductSearchModel
  { productSearchModelName :: Maybe String,
    productSearchModelPrice :: Maybe Double,
    productSearchModelColor :: Maybe Color,
    productSearchModelPage :: Int
  }

data ShopSearchModel = ShopSearchModel
  { shopSearchModelName :: Maybe String,
    shopSearchModelAddress :: Maybe String,
    shopSearchModelPage :: Int
  }

data CustomerSearchModel = CustomerSearchModel
  { customerSearchModelName :: Maybe String,
    customerSearchModelAddress :: Maybe String,
    customerSearchModelPage :: Int
  }

data OrderSearchModel = OrderSearchModel
  { orderSearchModelNumber :: Maybe String,
    orderSearchModelPage :: Int
  }