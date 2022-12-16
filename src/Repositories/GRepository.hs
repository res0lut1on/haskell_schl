{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repositories.GRepository (EntityName (..), GenericRepository (..), ReadWriteDataEntity (..)) where

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
import Control.Monad.Reader (MonadReader(ask))

class (BaseEntity a, ReadWriteDataEntity a) => GenericRepository a where
  getList :: App [a]
  getList = return []

  -- getList =
  --   tell ["Method getList begin"]
  --     >> get
  --     >>= \cache ->
  --       let cacheData = getCache cache :: [a]
  --        in getDataFromCache (not (null cacheData)) cache cacheData >>= \result ->
  --             tell ["Method getList end"]
  --               >> return result
  --   where
  --     getDataFromCache :: Bool -> AppCache -> [a] -> App [a]
  --     getDataFromCache True _ cache = return cache
  --     getDataFromCache False appCache _ = (readAllDataEntity (returnNameEntity (entityName :: EntityName a))) >>= \newCache -> put (setCache appCache newCache) >> retrun newCache

  --   name = returnNameEntity (entityName :: EntityName a)
  --  in readAllDataEntity name

  getEntityById :: Int -> App (Maybe a)
  getEntityById eid =
    tell ["Get begin"] >> maybeHead . filter (\e -> entId e == eid) <$> getList >>= \res ->
      tell ["Get end"] >> return res

  addEntity :: a -> App Int
  addEntity entity =
    tell ["AddEntity begin"] >> (\newId -> (\_ -> newId) (addNewEnt (entType entity) newId)) . getLastId <$> (getList :: IO [a]) >>= \res ->
      tell ["AddEntity end"] >> clearCache @a >> return res

  removeEid :: Int -> App (Maybe a)
  removeEid eid =
    let listEnt = (getList :: IO [a])
        ent = filter (\a -> entId a == eid) <$> listEnt
     in tell ["removeEid begin"] >> (writeAllDataEntity . filter (\a -> entId a /= eid) <$> listEnt) >> maybeHead <$> ent >>= \res -> isValid res >>= clearCache @a >> tell["removeEid end"] >> return res

  -- -- (getList :: IO [a]) >>= (writeAllDataEntity . filter (\a -> entId a /= eid))

  -- editEntity :: a -> App ()
  -- editEntity newEnt = tell ["editEntity begin"] >> (getList :: IO [a]) >>= ((writeAllDataEntity . addInTheEnd newEnt) . filter (\a -> entId a /= entId newEnt)) >>= \res -> 
  --   clearCache @a >> tell["editEntity end"] >> return res

  search :: (SearchModel b) => (b -> [a] -> [a]) -> b -> App [a]
  search filterModel searchModel = tell["search begin"] >> ask >>= \config ->  getList >>= (\res ->
    tell ["search end"] >> return res) . pagination (getPageNumber searchModel) (pageSize config) . filterModel searchModel

getLastId :: (BaseEntity a) => [a] -> Int
getLastId xs = entId (last xs) + 1
