{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Services.ShopServices (getModelShops, addModelShop, removeModelShop, editModelShop, getModelShopById, getShop) where

import Data.Entities (Product (..), Shop (..))
import Data.Models (ShopModel (ShopModel))
import Mapping.Mapping (mappingModelToShop, mappingShopToModel)
import Repositories.GenericRepository
import qualified Services.GService as S
import Startup
import Util.Utilities (toMaybeM)

getShop :: Int -> App ShopModel
getShop = S.get getProducts
  where
    getProducts :: Shop -> App (Maybe [Product])
    getProducts shp = toMaybeM $ getProductsByShop shp

getModelShops :: App [ShopModel]
getModelShops = map (`mappingShopToModel` Nothing) <$> getList

getModelShopById :: Int -> App ShopModel
getModelShopById smId =
  getEntityById smId >>= \shp ->
    getProductsByShop shp >>= \prods ->
      return $ mappingShopToModel shp (Just prods)

addModelShop :: ShopModel -> App Int
addModelShop item = addEntity $ mappingModelToShop item

removeModelShop :: (GenericRepository a) => Int -> App a
removeModelShop = removeEid

editModelShop :: ShopModel -> App ()
editModelShop item = editEntity $ mappingModelToShop item
