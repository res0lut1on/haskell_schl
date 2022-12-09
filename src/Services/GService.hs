module Services.GService (GenericService (..)) where

import Data.Entities
import Data.Models
import Data.RepEntity.BaseEntity (BaseEntity)
import Mapping.GMapping (GenericMapping (toList, toModel))
import Repositories.GRepository (GenericRepository (addEntity, editEntity, getEntityById))
import qualified Repositories.GRepository as GenericRepository

class (GenericMapping a, BaseEntity a) => GenericService a where
  getList :: IO [a]
  getList = toList <$> GenericRepository.getList

  get :: a -> Int -> IO c
  get getParams sid = toModel <$> getEntityById sid

  addModel :: a -> b -> IO Int
  addModel tmodel = addEntity <$> toModel tmodel

  editModel :: a -> IO a
  editModel tmodel = editEntity <*> toModel

getProduct :: Int -> IO (Maybe ProductModel)
getProduct =
  get getShop
  where
    getShop :: Product -> IO (Maybe ShopModel)
    getShop product =
      (get (productShopId product) :: IO (Maybe Shop)) >>= \maybeShop ->
        return
          ( maybeShop >>= \shop ->
              return $ toModel shop
          )
