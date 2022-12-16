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
import Startup
import Util.Utilities (unwrap)

-- getProduct :: Int -> App ProductModel
-- getProduct = S.get getShop
--   where
--     getShop :: Product -> App Shop
--     getShop = R.getEntityById . productShopId

getModelProducts :: App [ProductModel]
getModelProducts = map (`mappingProductToModel` Nothing) <$> getList

getModelProductById :: Int -> App ProductModel
getModelProductById prID =
  getEntityById
    prID
    >>= \prod ->
      getEntityById (productShopId prod)
        >>= \shp -> return $ mappingProductToModel prod (Just shp)

addModelProduct :: ProductModel -> App Int
addModelProduct item = addEntity $ mappingModelToProduct item

removeModelProduct :: (GenericRepository a) => Int -> App a
removeModelProduct = removeEid

editModelProduct :: ProductModel -> App ()
editModelProduct item = editEntity $ mappingModelToProduct item
