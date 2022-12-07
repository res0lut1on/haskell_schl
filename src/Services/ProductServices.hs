{-# LANGUAGE TypeApplications #-}

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
  do
    prod <- getEntityById prID
    case prod of
      Nothing -> return Nothing
      Just value ->
        do
          shp <- getEntityById $ productShopId value
          return $ Just $ mappingProductToModel value shp

addModelProduct :: ProductModel -> IO Int
addModelProduct item = addEntity $ mappingModelToProduct item

removeModelProduct :: Int -> IO ()
removeModelProduct = removeEid @Product

editModelProduct :: ProductModel -> IO ()
editModelProduct item = editEntity $ mappingModelToProduct item