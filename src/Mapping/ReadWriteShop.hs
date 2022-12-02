{-# OPTIONS_GHC -Wno-orphans #-}

module Mapping.ReadWriteShop () where

import Data.Entities (Shop)
import Mapping.ReadWriteEntityClass (ReadWriteDataEntity)

instance ReadWriteDataEntity Shop