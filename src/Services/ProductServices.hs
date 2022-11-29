module Services.ProductServices (getModelProducts, getModelProductById, addModelProduct, removeModelProduct, editModelProduct) where

import Data.Entities
import Data.Models (ProductModel)
import Mapping.Mapping
import Repositories.ProductRepository
import Repositories.ShopRepository

getModelProducts :: IO [ProductModel]
getModelProducts = map (`mappingProductToModel` Nothing) <$> Repositories.ProductRepository.getProducts

getModelProductById :: Int -> IO (Maybe ProductModel)
getModelProductById prID =
  do
    prod <- getProductById prID
    case prod of
      Nothing -> return Nothing
      Just value ->
        do
          shp <- getShopById $ productShopId value
          return $ Just $ mappingProductToModel value shp

addModelProduct :: ProductModel -> IO Int
addModelProduct item = addProduct $ mappingModelToProduct item

removeModelProduct :: Int -> IO ()
removeModelProduct = removeProduct

editModelProduct :: ProductModel -> IO ()
editModelProduct item = updateProduct $ mappingModelToProduct item