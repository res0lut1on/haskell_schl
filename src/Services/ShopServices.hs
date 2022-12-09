{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Services.ShopServices (getModelShops, addModelShop, removeModelShop, editModelShop, searchShops, getModelShopById) where

import Data.Entities
import Data.List
import Data.Models (ShopModel)
import Data.SearchModel (ShopSearchModel (ShopSearchModel, shopSearchModelAddress, shopSearchModelName, shopSearchModelPage))
import Mapping.Mapping (mappingModelToShop, mappingShopToModel)
import Repositories.GenericRepository
import Services.ApplyFilter

getModelShops :: IO [ShopModel]
getModelShops = map (`mappingShopToModel` Nothing) <$> getList

getModelShopById :: Int -> IO (Maybe ShopModel) -- про это я помню
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

searchShops :: ShopSearchModel -> IO [ShopModel]
searchShops model =
  map (`mappingShopToModel` Nothing) <$> search filterFunc model
  where
    filterFunc :: ShopSearchModel -> [Shop] -> [Shop]
    filterFunc shopSearchModel =
      applyFilter shopName shopSearchModelName isInfixOf shopSearchModel
        . applyFilter shopAddress shopSearchModelAddress isInfixOf shopSearchModel
