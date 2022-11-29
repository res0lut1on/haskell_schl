module Services.ShopServices (getModelShops, getModelShopById, addModelShop, removeModelShop, editModelShop) where

import Data.Models (ShopModel)
import Mapping.Mapping (mappingModelToShop, mappingShopToModel)
import Repositories.ProductRepository (getProductsByShop)
import Repositories.ShopRepository
  ( addShop,
    getShopById,
    removeShop,
    updateShop,
  )
import qualified Repositories.ShopRepository as ShopRepository

getModelShops :: IO [ShopModel]
getModelShops = map (`mappingShopToModel` Nothing) <$> ShopRepository.getShops

getModelShopById :: Int -> IO (Maybe ShopModel)
getModelShopById smId =
  do
    shop <- getShopById smId
    case shop of
      Nothing -> return Nothing
      Just value ->
        do
          prods <- getProductsByShop value
          return $ Just $ mappingShopToModel value (Just prods)

addModelShop :: ShopModel -> IO Int
addModelShop item = addShop $ mappingModelToShop item

removeModelShop :: Int -> IO ()
removeModelShop = removeShop

editModelShop :: ShopModel -> IO ()
editModelShop item = updateShop $ mappingModelToShop item