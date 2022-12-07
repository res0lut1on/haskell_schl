{-# OPTIONS_GHC -Wno-orphans #-}

module ReadWrite.ReadWriteOrder () where

import Data.Entities (Order)
import ReadWrite.ReadWriteEntityClass (ReadWriteDataEntity)

instance ReadWriteDataEntity Order