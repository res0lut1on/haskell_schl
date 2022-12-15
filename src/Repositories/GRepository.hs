{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repositories.GRepository (EntityName (..), GenericRepository (..), ReadWriteDataEntity (..)) where

import Data.RepEntity.BaseEntity (BaseEntity (..), EntityName (returnNameEntity))
import Data.SearchModel
import LibFold (addInTheEnd)
import ReadWrite.ReadWriteEntityClass (ReadWriteDataEntity (..))
import Services.ApplyFilter (pagination)
import Util.Utilities
import Startup (App)
import Control.Monad.Writer (MonadWriter(tell))

class (BaseEntity a, ReadWriteDataEntity a) => GenericRepository a where
  getList :: IO [a]
  getList =
    let name = returnNameEntity (entityName :: EntityName a)
     in readAllDataEntity name

  getEntityById :: Int -> IO (Maybe a)
  getEntityById eid =  maybeHead . filter (\e -> entId e == eid) <$> getList

  addEntity :: a -> IO Int
  addEntity entity = (\newId -> (\_ -> newId) (addNewEnt (entType entity) newId)) . getLastId <$> (getList :: IO [a])

  removeEid :: Int -> IO (Maybe a)
  removeEid eid =
    let listEnt = (getList :: IO [a])
        ent = filter (\a -> entId a == eid) <$> listEnt
     in (writeAllDataEntity . filter (\a -> entId a /= eid) <$> listEnt) >> maybeHead <$> ent

  -- (getList :: IO [a]) >>= (writeAllDataEntity . filter (\a -> entId a /= eid))

  editEntity :: a -> IO ()
  editEntity newEnt = (getList :: IO [a]) >>= ((writeAllDataEntity . addInTheEnd newEnt) . filter (\a -> entId a /= entId newEnt))

  search :: (SearchModel b) => (b -> [a] -> [a]) -> b -> IO [a]
  search filterModel searchModel = pagination (getPageNumber searchModel) (getPageSize searchModel) . filterModel searchModel <$> getList

getLastId :: (BaseEntity a) => [a] -> Int
getLastId xs = entId (last xs) + 1
