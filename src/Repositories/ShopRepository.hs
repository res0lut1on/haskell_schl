module Repositories.ShopRepository
  ( getShops,
    getShopById,
  )
where

import Data.Context (shops)
import Data.Entities (Shop (..))
import Lib

getShops :: [Shop]
getShops = shops

getShopById :: Int -> Shop -- Maybe
getShopById searchId = head $ Lib.filter (\x -> shopId x == searchId) shops

