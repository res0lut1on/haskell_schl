{-# LANGUAGE TypeApplications #-}

module Services.ShopServices (getModelShops, getModelShopById, addModelShop, removeModelShop, editModelShop) where

import Data.Entities
import Data.Models (ShopModel)
import Mapping.Mapping (mappingModelToShop, mappingShopToModel)
import Repositories.GenericRepository

getModelShops :: IO [ShopModel]
getModelShops = map (`mappingShopToModel` Nothing) <$> getList

getModelShopById :: Int -> IO (Maybe ShopModel)
getModelShopById smId =
  do
    shop <- getEntityById smId
    case shop of
      Nothing -> return Nothing
      Just value ->
        do
          prods <- getProductsByShop value
          return $ Just $ mappingShopToModel value (Just prods)

addModelShop :: ShopModel -> IO Int
addModelShop item = addEntity $ mappingModelToShop item

removeModelShop :: Int -> IO ()
removeModelShop = removeEid @Shop

editModelShop :: ShopModel -> IO ()
editModelShop item = editEntity $ mappingModelToShop item