{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Services.ProductServices (getModelProducts, getModelProductById, addModelProduct, removeModelProduct, editModelProduct) where

import Data.Entities
import Data.Models (ProductModel)
import Mapping.Mapping
  ( mappingModelToProduct,
    mappingProductToModel,
  )
import Repositories.GenericRepository

getModelProducts :: IO [ProductModel]
getModelProducts = map (`mappingProductToModel` Nothing) <$> getList

getModelProductById :: Int -> IO (Maybe ProductModel)
getModelProductById prID =
  getEntityById prID >>= \prod ->
    case prod of
      Nothing -> return Nothing
      Just value ->
        getEntityById (productShopId value) >>= \shp -> return $ Just $ mappingProductToModel value shp

addModelProduct :: ProductModel -> IO Int
addModelProduct item = addEntity $ mappingModelToProduct item

removeModelProduct :: Int -> IO ()
removeModelProduct = removeEid @Product

editModelProduct :: ProductModel -> IO ()
editModelProduct item = editEntity $ mappingModelToProduct item