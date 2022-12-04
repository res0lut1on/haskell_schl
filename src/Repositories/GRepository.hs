{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repositories.GRepository (EntityName (..), GenericRepository (..), ReadWriteDataEntity(..)) where

import Data.RepEntity.BaseEntity (BaseEntity (..), EntityName (returnNameEntity))
import ReadWrite.ReadWriteEntityClass ( ReadWriteDataEntity (..))
import LibFold (addInTheEnd)
import Util.Utilities

class (BaseEntity a, ReadWriteDataEntity a) => GenericRepository a where
  getList :: IO [a]
  getList = do
    let name = returnNameEntity (entityName :: EntityName a)
    readAllDataEntity name

  getEntityById :: Int -> IO (Maybe a)
  getEntityById eid = maybeHead . filter (\e -> entId e == eid) <$> getList

  addEntity :: a -> IO Int
  addEntity entity =
    do
      allEnt <- getList :: IO [a]
      let newId = getLastId allEnt
      addNewEnt (entType entity) newId
      return newId

  removeEid :: Int -> IO () -- ?????????????????????????????????????////
  removeEid eid =
    do
      oldAll <- getList :: IO [a]
      let newAll = filter (\a -> entId a /= eid) oldAll
      writeAllDataEntity newAll

  editEntity :: a -> IO ()
  editEntity newEnt =
    do
      oldAll <- getList :: IO [a]
      writeAllDataEntity (addInTheEnd newEnt $ filter (\a -> entId a /= entId newEnt) oldAll)

getLastId :: (BaseEntity a) => [a] -> Int
getLastId xs = entId (last xs) + 1
