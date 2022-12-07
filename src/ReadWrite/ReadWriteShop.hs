{-# OPTIONS_GHC -Wno-orphans #-}

module ReadWrite.ReadWriteShop () where

import Data.Entities (Shop)
import ReadWrite.ReadWriteEntityClass (ReadWriteDataEntity)

instance ReadWriteDataEntity Shop