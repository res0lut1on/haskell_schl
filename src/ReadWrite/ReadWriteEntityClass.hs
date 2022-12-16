{-# LANGUAGE InstanceSigs #-}

module ReadWrite.ReadWriteEntityClass (ReadWriteDataEntity (..)) where

import Data.MappingTxt
import Data.RepEntity.BaseEntity (BaseEntity (..))
import Lib
import Util.FileUtil
import Startup

class (BaseEntity a, MappEntity a) => ReadWriteDataEntity a where
  addNewEnt :: a -> Int -> App ()
  addNewEnt ent newId = addEnt (changeId ent newId)

  writeAllDataEntity :: [a] -> App ()
  writeAllDataEntity = writeEntWhile

  readAllDataEntity :: String -> App [a]
  readAllDataEntity ent = Lib.map mappEntityFrom <$> (lines <$> readEntityData ent)