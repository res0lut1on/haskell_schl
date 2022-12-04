{-# OPTIONS_GHC -Wno-orphans #-}

module ReadWrite.ReadWriteCustomer () where

import Data.Entities
import ReadWrite.ReadWriteEntityClass (ReadWriteDataEntity)

instance ReadWriteDataEntity Customer