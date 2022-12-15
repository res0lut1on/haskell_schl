{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Services.ProductServices (getModelProducts, getModelProductById, addModelProduct, removeModelProduct, editModelProduct, getProduct) where

import Data.Entities (Product (Product), Shop (Shop), productShopId)
import Data.Models (ProductModel, ShopModel (ShopModel))
import Mapping.GMapping
import Mapping.Mapping
  ( mappingModelToProduct,
    mappingProductToModel,
  )
import Mapping.MappingParam ()
import Repositories.GenericRepository as R
import Services.ApplyFilter ()
import qualified Services.GService as S
import Util.Utilities (unwrap)

getProduct :: Int -> IO (Maybe ProductModel)
getProduct = S.get getShop
  where
    getShop :: Product -> IO (Maybe Shop)
    getShop = R.getEntityById . productShopId









getModelProducts :: IO [ProductModel]
getModelProducts = map (`mappingProductToModel` Nothing) <$> getList

getModelProductById :: Int -> IO (Maybe ProductModel)
getModelProductById prID =
  unwrap $
    getEntityById
      prID
      >>= \maybeProd ->
        return $
          maybeProd >>= \prod ->
            return $
              getEntityById (productShopId prod)
                >>= \shp -> return $ Just $ mappingProductToModel prod shp

addModelProduct :: ProductModel -> IO Int
addModelProduct item = addEntity $ mappingModelToProduct item

removeModelProduct :: (GenericRepository a) => Int -> IO (Maybe a)
removeModelProduct = removeEid

editModelProduct :: ProductModel -> IO ()
editModelProduct item = editEntity $ mappingModelToProduct item
