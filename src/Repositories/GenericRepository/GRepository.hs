-- это законно ?
{-# LANGUAGE InstanceSigs #-}

module Repositories.GenericRepository.GRepository (EntityName (..), GenericRepository (..)) where

import Data.Entities
import Data.RepEntity.BaseEntity (BaseEntity (..))
import Mapping.ReadWriteEntityClass (ReadWriteDataEntity (..))
import Util.Utilities (maybeHead)

-- entityName:: EntityName a

-- instance Eq BaseEntity where
--   (==) ::(BaseEntity a) => a -> a -> Bool
--   x == y = entId x == entId y

--------------------------------------------------------------------------------------

class (BaseEntity a, ReadWriteDataEntity a) => GenericRepository a where
  -- getList :: a -> IO [a]
  -- getList entity = readAllDataEntity (entType entity)

  getList :: IO [a]
  getList = do
    let name = returnNameEntity (entityName :: EntityName a)
    return $ readAllDataEntity name

  getEid :: a -> Int -> IO (Maybe a)
  getEid entity eid = maybeHead . filter (\e -> entId e == eid) <$> getList entity

  addEntity :: a -> IO ()
  addEntity entity =
    do
      allEnt <- getList entity
      let newId = getLastId allEnt
      addNewEnt (entType entity) newId

  deleteEid :: a -> Int -> IO ()
  deleteEid entity eid =
    do
      oldAll <- getList entity
      let newAll = filter (\a -> entId a /= eid) oldAll
      writeAllDataEntity newAll

  editEntity :: a -> IO ()
  editEntity newEnt =
    do
      deleteEid newEnt (entId newEnt)
      addEntity newEnt

getLastId :: (BaseEntity a) => [a] -> Int
getLastId xs = entId (last xs) + 1

-------------------------------------------------------------------------------------
