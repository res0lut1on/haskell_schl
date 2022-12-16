{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Repositories.GRepository (EntityName (..), GenericRepository (..), ReadWriteDataEntity (..)) where

import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.State
import Control.Monad.Writer (MonadWriter (tell))
import Data.RepEntity.BaseEntity (BaseEntity (..), EntityName (returnNameEntity))
import Data.SearchModel
import LibFold (addInTheEnd)
import ReadWrite.ReadWriteEntityClass (ReadWriteDataEntity (..))
import Services.ApplyFilter (pagination)
import Startup (App, AppConfig (pageSize))
import Util.CacheStyle
import Util.Utilities

class (BaseEntity a, ReadWriteDataEntity a, CacheStyle a, Show a) => GenericRepository a where
  getList :: App [a]
  getList =
    tell ["Method getList begin"]
      >> get
      >>= \cache ->
        let cacheData = getCache cache :: [a]
         in tell ["Method getList entity = " ++ getNameEnt @a]
              >> ( if null cacheData
                     then
                       readAllDataEntity (getNameEnt @a)
                         >>= \newCache ->
                           put (setCache cache newCache)
                             >> tell ["Method getList end"]
                             >> return newCache
                     else
                       tell ["Method getList end"]
                         >> return cacheData
                 )

  getEntityById :: Int -> App a
  getEntityById eid =
    let appArrEnt = filter (\e -> entId e == eid) <$> getList :: App [a]
     in tell ["Get begin"]
          >> tell ["Get with entity = " ++ getNameEnt @a]
          >> appArrEnt
          >>= ( isValidArr "getEntityById" eid
                  >=> (\res -> tell ["GetEntityById with id = " ++ show eid] >> tell ["Get end"] >> return res)
              )

  addEntity :: a -> App Int
  addEntity entity =
    let newId = getLastId <$> (getList :: App [a])
     in tell ["AddEntity begin"] >> newId >>= \res ->
          addNewEnt (entType entity) res
            >> tell ["AddEntity with entity = " ++ getNameEnt @a]
            >> tell ["AddEntity entity with id = " ++ show (entId entity)]
            >> clearCache @a
            >> tell ["AddEntity end"]
            >> newId

  removeEid :: Int -> App a
  removeEid eid =
    let listEnt = (getList :: App [a])
        appArrEnt = filter (\a -> entId a == eid) <$> listEnt
     in tell ["removeEid begin"]
          >> appArrEnt
          >>= ( isValidArr "Edit" eid
                  >=> ( \res ->
                          ( writeAllDataEntity . filter (\a -> entId a /= eid)
                              <$> listEnt
                          )
                            >> tell ["removeEid with entity = " ++ getNameEnt @a]
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
          isValidArr "Edit" (entId newEnt) arrEnt
            >> tell ["editEntity with entity = " ++ getNameEnt @a]
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

getNameEnt :: forall a. (BaseEntity a) => String
getNameEnt = returnNameEntity (entityName :: EntityName a)
