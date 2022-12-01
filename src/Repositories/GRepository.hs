
{-# LANGUAGE InstanceSigs #-} -- это законно ?

import Data.Entities
import Util.FileUtil
import Util.Utilities (maybeHead)

class BaseEntity a where
  entType :: a -> a
  entId :: a -> Int

-- instance Eq BaseEntity where
--   (==) ::(BaseEntity a) => a -> a -> Bool
--   x == y = entId x == entId y

class ReadWriteDataEntity a where
  readAllDataEntity :: a -> IO [a]
  addNewEnt :: a -> Int -> IO ()
  writeAllDataEntity :: [a] -> IO ()

instance ReadWriteDataEntity Customer where
  readAllDataEntity :: Customer -> IO [Customer]
  readAllDataEntity _ = readAllCustomers
  addNewEnt :: Customer -> Int -> IO ()
  addNewEnt cust newId = addCust (Customer newId (customerName cust) (customerAddress cust))
  writeAllDataEntity :: [Customer] -> IO ()
  writeAllDataEntity = writeCustWhile

instance ReadWriteDataEntity Shop where
  readAllDataEntity :: Shop -> IO [Shop]
  readAllDataEntity _ = readAllShops
  addNewEnt :: Shop -> Int -> IO ()
  addNewEnt shp newId = addShp (Shop newId (shopName shp) (shopAddress shp))
  writeAllDataEntity :: [Shop] -> IO ()
  writeAllDataEntity = writeShopWhile

instance BaseEntity Customer where
  entType :: Customer -> Customer
  entType s = s
  entId :: Customer -> Int
  entId (Customer cId _ _) = cId

instance BaseEntity Shop where
  entType :: Shop -> Shop
  entType s = s
  entId :: Shop -> Int
  entId (Shop sId _ _) = sId

class (BaseEntity a, ReadWriteDataEntity a) => ReposGenericRepository a where
  getList :: a -> IO [a]
  getList entity = readAllDataEntity (entType entity)

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

getLastId :: (BaseEntity a) => [a] -> Int -- generic для pk?
getLastId xs = entId (last xs) + 1

-------------------------------------------------------------------------------------
class MappEntity a where
  mappEntityTo :: a -> String
  mappEntityFrom :: String -> a

instance MappEntity Shop where
  mappEntityTo :: Shop -> String
  mappEntityTo = mappShopToTxt
  mappEntityFrom :: String -> Shop
  mappEntityFrom = mappShopFromTxt