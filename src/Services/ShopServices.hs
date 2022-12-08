{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Services.ShopServices (getModelShops, getModelShopById, addModelShop, removeModelShop, editModelShop) where

import Data.Entities
import Data.Models (ShopModel)
import Mapping.Mapping (mappingModelToShop, mappingShopToModel)
import Repositories.GenericRepository

getModelShops :: IO [ShopModel]
getModelShops = map (`mappingShopToModel` Nothing) <$> getList

getModelShopById :: Int -> IO (Maybe ShopModel)
getModelShopById smId =
  getEntityById smId >>= \shop ->
    case shop of
      Nothing -> return Nothing
      Just value ->
        getProductsByShop value >>= \prods ->
          return $ Just $ mappingShopToModel value (Just prods)

addModelShop :: ShopModel -> IO Int
addModelShop item = addEntity $ mappingModelToShop item

removeModelShop :: Int -> IO ()
removeModelShop = removeEid @Shop

editModelShop :: ShopModel -> IO ()
editModelShop item = editEntity $ mappingModelToShop item