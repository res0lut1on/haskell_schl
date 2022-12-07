{-# OPTIONS_GHC -Wno-orphans #-}

module ReadWrite.ReadWriteProduct () where

import Data.Entities (Product)
import ReadWrite.ReadWriteEntityClass (ReadWriteDataEntity (..))

instance ReadWriteDataEntity Product