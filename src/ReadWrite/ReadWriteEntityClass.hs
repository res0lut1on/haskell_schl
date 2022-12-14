{-# LANGUAGE InstanceSigs #-}

module ReadWrite.ReadWriteEntityClass (ReadWriteDataEntity (..)) where

import Data.MappingTxt
import Data.RepEntity.BaseEntity (BaseEntity (..))
import Lib
import Util.FileUtil

class (BaseEntity a, MappEntity a) => ReadWriteDataEntity a where
  addNewEnt :: a -> Int -> IO ()
  addNewEnt ent newId = addEnt (changeId ent newId)

  writeAllDataEntity :: [a] -> IO ()
  writeAllDataEntity = writeEntWhile

  readAllDataEntity :: String -> IO [a]
  readAllDataEntity ent = Lib.map mappEntityFrom <$> (lines <$> readEntityData ent)