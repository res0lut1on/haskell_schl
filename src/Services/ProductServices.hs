{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Services.ProductServices (getModelProducts, getModelProductById, addModelProduct, removeModelProduct, editModelProduct, searchProducts) where

import Data.Entities
import Data.List
import Data.Models (ProductModel)
import Data.SearchModel
import Mapping.Mapping
  ( mappingModelToProduct,
    mappingProductToModel,
  )
import Repositories.GenericRepository
import Services.ApplyFilter
import Util.Utilities (unwrap)

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

removeModelProduct :: Int -> IO ()
removeModelProduct = removeEid @Product

editModelProduct :: ProductModel -> IO ()
editModelProduct item = editEntity $ mappingModelToProduct item

searchProducts :: ProductSearchModel -> IO [ProductModel]
searchProducts model =
  map (`mappingProductToModel` Nothing) <$> search filterFunc model
  where
    filterFunc :: ProductSearchModel -> [Product] -> [Product]
    filterFunc filters =
      applyFilter productName productSearchModelName isInfixOf filters
        . applyFilter productPrice productSearchModelPrice (==) filters
        . applyFilter productColor productSearchModelColor (==) filters
