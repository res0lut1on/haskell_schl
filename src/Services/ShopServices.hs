{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Services.ShopServices (getModelShops, addModelShop, removeModelShop, editModelShop, getModelShopById, getShop) where

import Data.Entities (Product (..), Shop (..))
import Data.Models (ShopModel (ShopModel))
import Mapping.Mapping (mappingModelToShop, mappingShopToModel)
import Repositories.GenericRepository
import qualified Services.GService as S
import Util.Utilities (unwrap)

getShop :: Int -> IO (Maybe ShopModel)
getShop = S.get getProducts
  where
    getProducts :: Shop -> IO (Maybe [Product])
    getProducts shp = Just <$> getProductsByShop shp

getModelShops :: IO [ShopModel]
getModelShops = map (`mappingShopToModel` Nothing) <$> getList

getModelShopById :: Int -> IO (Maybe ShopModel)
getModelShopById smId =
  unwrap $
    getEntityById smId >>= \maybeShop ->
      return $
        maybeShop >>= \shp ->
          return $
            getProductsByShop shp >>= \prods ->
              return $ Just $ mappingShopToModel shp (Just prods)

addModelShop :: ShopModel -> IO Int
addModelShop item = addEntity $ mappingModelToShop item

removeModelShop :: (GenericRepository a) => Int -> IO (Maybe a)
removeModelShop = removeEid

editModelShop :: ShopModel -> IO ()
editModelShop item = editEntity $ mappingModelToShop item
