module Repositories.ShopRepository
  ( getShops,
    getShopById,
    addShop,
    updateShop,
    removeShop,
  )
where

import Data.Entities (Shop (..))
import Util.FileUtil
import Util.Utilities (maybeHead)

getShops :: IO [Shop]
getShops = readAllShops

getShopById :: Int -> IO (Maybe Shop)
getShopById searchId = maybeHead . filter (\x -> shopId x == searchId) <$> readAllShops

addShop :: Shop -> IO Int
addShop item =
  do
    allItem <- readAllShops
    let prevItem = last allItem
    let iId = shopId prevItem + 1
    addShp
      ( Shop
          { shopId = shopId item,
            shopName = shopName item,
            shopAddress = shopAddress item
          }
      )
    return iId

removeShop :: Int -> IO ()
removeShop = removeShp

updateShop :: Shop -> IO ()
updateShop = updateShp