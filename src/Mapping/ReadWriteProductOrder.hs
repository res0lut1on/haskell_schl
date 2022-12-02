{-# OPTIONS_GHC -Wno-orphans #-}

module Mapping.ReadWriteProductOrder () where

import Data.Entities (ProductOrder)
import Mapping.ReadWriteEntityClass (ReadWriteDataEntity)

instance ReadWriteDataEntity ProductOrder