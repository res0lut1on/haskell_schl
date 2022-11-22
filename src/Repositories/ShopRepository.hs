module Repositories.ShopRepository
  ( getShops,
    getShopById,
  )
where

import Data.Context (shops)
import Data.Entities (Shop (..))
import Lib
import Utilities (maybeHead)

getShops :: [Shop]
getShops = shops

getShopById :: Int -> Maybe Shop
getShopById searchId = maybeHead $ Lib.filter (\x -> shopId x == searchId) getShops
