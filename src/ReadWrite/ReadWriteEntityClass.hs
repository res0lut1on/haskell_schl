{-# LANGUAGE InstanceSigs #-}

module ReadWrite.ReadWriteEntityClass (ReadWriteDataEntity (..)) where

import Data.RepEntity.BaseEntity (BaseEntity(..))
import Mapping.MappingTxt
import Util.FileUtil
import Lib

class (BaseEntity a, MappEntity a) => ReadWriteDataEntity a where
  addNewEnt :: a -> Int -> IO ()
  addNewEnt ent newId = addEnt (changeId ent newId)
  writeAllDataEntity :: [a] -> IO ()
  writeAllDataEntity = writeEntWhile
  readAllDataEntity :: String -> IO [a]
  readAllDataEntity ent =
    do
      contents <- readEntityData ent
      return $ Lib.map mappEntityFrom (lines contents)
