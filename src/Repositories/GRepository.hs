{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repositories.GRepository (EntityName (..), GenericRepository (..), ReadWriteDataEntity (..)) where

import Data.RepEntity.BaseEntity (BaseEntity (..), EntityName (returnNameEntity))
import LibFold (addInTheEnd)
import ReadWrite.ReadWriteEntityClass (ReadWriteDataEntity (..))
import Util.Utilities

class (BaseEntity a, ReadWriteDataEntity a) => GenericRepository a where
  getList :: IO [a]
  getList =
    let name = returnNameEntity (entityName :: EntityName a)
     in readAllDataEntity name

  getEntityById :: Int -> IO (Maybe a)
  getEntityById eid = maybeHead . filter (\e -> entId e == eid) <$> getList

  addEntity :: a -> IO Int
  addEntity entity = (\newId -> (\_ -> newId) (addNewEnt (entType entity) newId)) . getLastId <$> (getList :: IO [a])

  -- (getList :: IO [a]) >>= ((\newId -> (\_ -> return newId) (addNewEnt (entType entity) newId)) . getLastId)
  removeEid :: Int -> IO ()
  removeEid eid = (getList :: IO [a]) >>= (writeAllDataEntity . filter (\a -> entId a /= eid))

  editEntity :: a -> IO ()
  editEntity newEnt = (getList :: IO [a]) >>= ((writeAllDataEntity . addInTheEnd newEnt) . filter (\a -> entId a /= entId newEnt))

getLastId :: (BaseEntity a) => [a] -> Int
getLastId xs = entId (last xs) + 1
