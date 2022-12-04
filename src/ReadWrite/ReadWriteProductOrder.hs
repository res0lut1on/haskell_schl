{-# OPTIONS_GHC -Wno-orphans #-}

module ReadWrite.ReadWriteProductOrder () where

import Data.Entities (ProductOrder)
import ReadWrite.ReadWriteEntityClass (ReadWriteDataEntity)

instance ReadWriteDataEntity ProductOrder