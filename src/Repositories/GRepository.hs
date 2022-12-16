{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Repositories.GRepository (EntityName (..), GenericRepository (..), ReadWriteDataEntity (..)) where

import Control.Monad ((>=>))
import qualified Control.Monad
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.State
import Control.Monad.Writer (MonadWriter (tell))
import Data.RepEntity.BaseEntity (BaseEntity (..), EntityName (returnNameEntity))
import Data.SearchModel
import LibFold (addInTheEnd)
import ReadWrite.ReadWriteEntityClass (ReadWriteDataEntity (..))
import Services.ApplyFilter (pagination)
import Startup (App, AppCache, AppConfig (pageSize))
import Util.CacheStyle
import Util.Utilities

class (BaseEntity a, ReadWriteDataEntity a, CacheStyle a) => GenericRepository a where
  getList :: App [a]
  getList =
    tell ["Method getList begin"]
      >> get
      >>= \cache ->
        let cacheData = getCache cache :: [a]
         in getDataFromCache (not (null cacheData)) cache cacheData >>= \result ->
              tell ["Method getList end"]
                >> return result
    where
      getDataFromCache :: Bool -> AppCache -> [a] -> App [a]
      getDataFromCache True _ cache = return cache
      getDataFromCache False appCache _ = readAllDataEntity (returnNameEntity (entityName :: EntityName a)) >>= \newCache -> put (setCache appCache newCache) >> return newCache

  getEntityById :: Int -> App a
  getEntityById eid =
    let appArrEnt = filter (\e -> entId e == eid) <$> getList :: App [a]
     in tell ["Get begin"]
          >> appArrEnt
          >>= ( isValidArr
                  >=> (\res -> tell ["Get end"] >> return res)
              )

  addEntity :: a -> App Int
  addEntity entity =
    let newId = getLastId <$> (getList :: App [a])
     in tell ["AddEntity begin"] >> newId >>= \res ->
          addNewEnt (entType entity) res
            >> tell ["AddEntity add"]
            >> clearCache @a
            >> tell ["AddEntity end"]
            >> newId

  removeEid :: Int -> App a
  removeEid eid =
    let listEnt = (getList :: App [a])
        appArrEnt = filter (\a -> entId a == eid) <$> listEnt
     in tell ["removeEid begin"]
          >> appArrEnt
          >>= ( isValidArr
                  >=> ( \res ->
                          ( writeAllDataEntity . filter (\a -> entId a /= eid)
                              <$> listEnt
                          )
                            >> tell ["removeEid remove ent with id = " ++ show eid]
                            >> clearCache @a
                            >> tell ["removeEid end"]
                            >> return res
                      )
              )

  editEntity :: a -> App ()
  editEntity newEnt =
    let appArrEnt = filter (\a -> entId a == entId newEnt) <$> (getList :: App [a])
     in tell ["editEntity begin"] >> appArrEnt >>= \arrEnt ->
          isValidArr arrEnt
            >> writeAllDataEntity (addInTheEnd newEnt arrEnt)
            >> tell ["editEntity edit complete with entity " ++ show (entId newEnt)]
            >> clearCache @a
            >> tell ["editEntity end"]

  search :: (SearchModel b) => (b -> [a] -> [a]) -> b -> App [a]
  search filterModel searchModel =
    tell ["search begin"] >> ask >>= \config ->
      getList
        >>= ( \res ->
                tell ["search end"] >> return res
            )
          . pagination (getPageNumber searchModel) (pageSize config)
          . filterModel searchModel

getLastId :: (BaseEntity a) => [a] -> Int
getLastId xs = entId (last xs) + 1
